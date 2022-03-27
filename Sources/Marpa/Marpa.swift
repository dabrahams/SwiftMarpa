import libmarpa

let initialConfig: Marpa_Config = {
  var c: Marpa_Config = .init()
  marpa_c_init(&c)
  return c
}()

public final class Grammar {
  var g: Marpa_Grammar
  public init() {
    var c: Marpa_Config = initialConfig
    g = marpa_g_new(&c)
    checkCode(marpa_c_error(&c, nil))
    checkCode(marpa_g_force_valued(g))
  }
  
  deinit {
    marpa_g_unref(g)
  }
}

func fatal(_ x: Int32) -> Never {
  fatalError("Marpa error: \(errorDescription[x]!)")
}

/// Result handling
extension Grammar {
  var err: Int32 { marpa_g_error(g, nil) }
  
  func ptr<T>(_ x: UnsafeMutablePointer<T>?) -> UnsafeMutablePointer<T> {
    if let r = x { return r }
    fatal(err)
  }
  
  func std(_ x: Int32) -> UInt32 {
    if x >= 0 { return UInt32(x) }
    fatal(err)
  }
  
  func stdOpt(_ x: Int32) -> UInt32? {
    x == -1 ? nil : std(x)
  }

  func checkCode(_ x: Int32) {
    if x != MARPA_ERR_NONE { fatal(x) }
  }
  
  func hidden(_ x: Int32) -> Int32 {
    checkCode(err)
    return x
  }
}

public struct Symbol: Hashable {
  public typealias ID = UInt32
  let id: ID
  var rawID: Marpa_Symbol_ID { .init(truncatingIfNeeded: id) }
}

/// Symbols
extension Grammar {
  public typealias Terminal = Symbol
  public typealias Nonterminal = Symbol

  /// Returns a new symbol ID in this grammar.
  func makeSymbolID(isTerminal: Bool) -> Symbol.ID {
    let rawID = marpa_g_symbol_new(g)
    let r = std(rawID)
    _ = std(marpa_g_symbol_is_terminal_set(g, rawID, isTerminal ? 1 : 0))
    return r
  }

  /// Returns a new terminal symbol in this grammar.
  func makeTerminal() -> Terminal {
    Terminal(id: makeSymbolID(isTerminal: true))
  }

  /// Returns a new terminal symbol in this grammar.
  func makeNonterminal() -> Nonterminal {
    Nonterminal(id: makeSymbolID(isTerminal: false))
  }

  /// Returns the start symbol, or `nil` if none has been set.
  public var startSymbol: Nonterminal? {
    get {
      guard let r = stdOpt(marpa_g_start_symbol(g)) else { return nil }
      return Nonterminal(id: r)
    }
    set {
      _ = std(marpa_g_start_symbol_set(g, newValue?.rawID ?? -2))
    }
  }

  /// The number of symbols in `self`.
  ///
  /// Symbol IDs are in `0..<symbolCount`.
  public var symbolCount: Int {
    Int(stdOpt(marpa_g_highest_symbol_id(g)) ?? 0) + 1
  }

  /// Returns `true` iff `s` can participate in a complete parse of the start
  /// symbol.
  public func isAccessible(_ s: Symbol) -> Bool {
    std(marpa_g_symbol_is_accessible(g, s.rawID)) != 0
  }

  /// Returns `true` iff `s` can be recognized from an empty sequence of tokens.
  public func isNullable(_ s: Symbol) -> Bool {
    std(marpa_g_symbol_is_nullable(g, s.rawID)) != 0
  }

  /// Returns `true` iff `s` can be recognized only from an empty sequence of
  /// tokens.
  public func isNulling(_ s: Symbol) -> Bool {
    std(marpa_g_symbol_is_nulling(g, s.rawID)) != 0
  }

  /// Returns `true` iff `s` can be recognized.
  public func isProductive(_ s: Symbol) -> Bool {
    std(marpa_g_symbol_is_productive(g, s.rawID)) != 0
  }

  // unneeded; we can compare with startSymbol
  // int symbol_is_start ( Marpa_Symbol_ID sym_id);
}

/// Rules
extension Grammar {
  public struct Rule: Hashable {
    public typealias ID = UInt32
    public let id: ID
    public var rawID: Marpa_Rule_ID { .init(truncatingIfNeeded: id) }
  }

  /// The number of rules in `self`.
  ///
  /// Rule IDs are in `0..<ruleCount`.
  public var ruleCount: Int {
    Int(stdOpt(marpa_g_highest_rule_id(g)) ?? 0) + 1
  }

  /// Returns true iff `r` can participate in a parse of the start symbol.
  public func isAccessible(_ r: Rule) -> Bool {
    std(marpa_g_rule_is_accessible(g, r.rawID)) != 0
  }

  /// Returns true iff `r` can recognize an empty sequence of tokens.
  public func isNullable(_ r: Rule) -> Bool {
    std(marpa_g_rule_is_nullable(g, r.rawID)) != 0
  }

  /// Returns `true` iff `r` can be recognized only from an empty sequence of
  /// tokens.
  public func isNulling(_ r: Rule) -> Bool {
    std(marpa_g_rule_is_nulling(g, r.rawID)) != 0
  }

  /// Returns `true` iff a non-empty series of reductions including `r`
  /// transforms `r`'s LHS symbol back into itself.
  ///
  /// The presence of a loop rule makes a grammar infinitely ambiguous, and
  /// applications will typically want to treat them as fatal errors. But
  /// nothing forces an application to do this, and Marpa will successfully
  /// parse and evaluate grammars with loop rules.
  public func isLoop(_ r: Rule) -> Bool {
    std(marpa_g_rule_is_loop(g, r.rawID)) != 0
  }

  /// Returns `true` iff `r` can be recognized.
  public func isProductive(_ r: Rule) -> Bool {
    std(marpa_g_rule_is_productive(g, r.rawID)) != 0
  }

  /// Returns the number of symbols in `r`'s RHS.
  public func rhsCount(_ r: Rule) -> Int {
    Int(std(marpa_g_rule_length(g, r.rawID)))
  }

  /// Returns the `r`'s LHS symbol.
  public func lhs(_ r: Rule) -> Nonterminal {
    Nonterminal(id: std(marpa_g_rule_lhs(g, r.rawID)))
  }

  /// Returns a new rule in `self` reducing the symbols in `rhs` to `lhs`.
  public func makeRule<RHS: Collection>(lhs: Nonterminal, rhs: RHS) -> Rule
    where RHS.Element == Symbol.ID
  {
    var rhsIDs = Array(rhs.lazy.map { Marpa_Symbol_ID(truncatingIfNeeded: $0) })
    return Rule(
      id: std(marpa_g_rule_new(g, lhs.rawID, &rhsIDs, Int32(rhsIDs.count))))
  }

  /// Returns the `r`'s RHS symbol IDs.
  public func rhsIDs(_ r: Rule) -> LazyMapSequence<Range<Int>, UInt32> {
    (0..<rhsCount(r)).lazy.map {
      self.std(marpa_g_rule_rhs(self.g, r.rawID, Int32($0)))
    }
  }
}

/// Sequence rules
extension Grammar {
  /// Returns `true` iff `r` is a sequence rule that doesn't recognize a
  /// trailing separator.
  public func isProperSeparation(_ r: Rule) -> Bool {
    std(marpa_g_rule_is_proper_separation(g, r.rawID)) != 0
  }

  /// Returns the minimum number of sequence repetitions recognized by `r`, or
  /// `nil` if `r` is not a sequence rule.
  public func sequenceMinRepetitions(_ r: Rule) -> Int? {
    stdOpt(marpa_g_sequence_min(g, r.rawID)).map { Int($0) }
  }

  /// Returns a new rule in `self`, reducing sequences of `rhs` (separated by
  /// `separator` if non-`nil`), to `lhs`.
  ///
  /// - Precondition: `lhs` is not the LHS symbol of any other rule.
  /// - Precondition: `rhs` is not a nullable symbol.
  /// - Parameter nullable: `true` iff the empty sequence is recognized.
  /// - Parameter trailingSeparatorAllowed: `true` if an optional final instance
  ///   of the separator is recognized.
  ///
  /// - Note: sequence rules can always be represented by (often less-efficient)
  ///   equivalent combinations of non-sequence rules.
  public func makeSequenceRule(
    lhs: Nonterminal, rhs: Symbol.ID, nullable: Bool,
    separator: Symbol.ID? = nil, trailingSeparatorAllowed: Bool = false
  ) -> Rule {
    Rule(
      id: std(
        marpa_g_sequence_new(
          g, lhs.rawID, Marpa_Symbol_ID(truncatingIfNeeded: rhs),
          separator.map { Marpa_Symbol_ID(truncatingIfNeeded: $0) } ?? -1,
          nullable ? 0 : 1,
          trailingSeparatorAllowed ? MARPA_PROPER_SEPARATION : 0)))
  }

  /// Returns the separator of the sequence rule `r`, or `nil` if `r` has no
  /// separator.
  ///
  /// - Precondition: `r` is a sequence rule
  public func sequenceSeparator(_ r: Rule) -> Symbol.ID? {
    stdOpt(marpa_g_sequence_separator(g, r.rawID))
  }

  /// Returns `true` iff `s` participates in the RHS of a sequence rule, either
  /// as the primary RHS symbol or as a separator.
  public func isCountedInSequence(_ s: Symbol) -> Bool {
    std(marpa_g_symbol_is_counted(g, s.rawID)) != 0
  }
}

/// Ranks
extension Grammar.Rule {
  public typealias Rank = Marpa_Rank
}

/// Ranks
extension Grammar {

  /// A writable mapping from a `Rule` to its rank.
  public struct RankMap {
    let g: Grammar

    /// Accesses the ranking of `r`.
    public subscript(r: Rule) -> Int32 {
      get {
        let r = marpa_g_rule_rank(g.g, r.rawID)
        return r != -2 ? r : g.hidden(r)
      }
      nonmutating set {
        if marpa_g_rule_rank_set(g.g, r.rawID, newValue) == -2 {
          g.checkCode(g.err)
        }
      }
    }
  }

  /// The ranking of each rule.
  var rank: RankMap { .init(g: self) }

  /// A writable mapping from a `Rule` to its “ranks high” flag.
  public struct RanksHighMap {
    let g: Grammar

    /// Accesses the “ranks high” flag of `r`.
    public subscript(r: Rule) -> Bool {
      get {
        g.std(marpa_g_rule_null_high(g.g, r.rawID)) != 0
      }
      nonmutating set {
        _ = g.std(marpa_g_rule_null_high_set(g.g, r.rawID, newValue ? 1 : 0))
      }
    }
  }
  
  /// The “null ranks high” flag of each rule.
  var nullRanksHigh: RanksHighMap { .init(g: self) }

  /*
  /// Sets the rank of `r` to `newRank`.
  public func setRank(of r: Rule, to newRank: Rule.Rank) {
    if marpa_g_rule_rank_set(g, r.rawID, newRank) == -2 {
      checkCode(err)
    }
  }
  
  /// Returns the rank of `r`.
  public func rank(of r: Rule) -> Rule.Rank {
    let r = marpa_g_rule_rank(g, r.rawID)
    return r != -2 ? r : hidden(r)
  }

  /// Sets the “null ranks high” attribute of `r` to `newRanksHigh`.
  public func setNullRanksHigh(of r: Rule, to newRanksHigh: Bool) {
    _ = std(marpa_g_rule_null_high_set(g, r.rawID, newRanksHigh ? 1 : 0))
  }
  
  /// Returns the “null ranks high” attribute of `r`.
  public func getNullRanksHigh(of r: Rule) -> Bool {
    std(marpa_g_rule_null_high(g, r.rawID)) != 0
  }
   */
}

/// Completion Events
extension Grammar {
  /// Enables the completion event trigger for `s`.
  ///
  /// - Precondition: `canTriggerCompletionEvent[s]``
  public func enableCompletionEvent(_ s: Symbol) {
    _ = std(marpa_g_completion_symbol_activate(g, s.rawID, 1))
  }
  
  /// Disables the completion event trigger for `s`.
  ///
  /// - Precondition: `canTriggerCompletionEvent[s]``
  public func disableCompletionEvent(_ s: Symbol) {
    _ = std(marpa_g_completion_symbol_activate(g, s.rawID, 0))
  }

  /// A writable mapping from a `Symbol` to whether a completion event can be
  /// triggered for it.
  public struct CompletionTriggerMap {
    let g: Grammar

    /// Accesses the “has completion event” flag of `s`.
    ///
    /// - Precondition(set): `!self.isPrecomputed`
    public subscript(_ s: Symbol) -> Bool {
      get {
        g.std(marpa_g_symbol_is_completion_event(g.g, s.rawID)) != 0
      }
      nonmutating set {
        _ = g.std(
          marpa_g_symbol_is_completion_event_set(g.g, s.rawID, newValue ? 1 : 0))
      }
    }
  }

  /// The completion event trigger capability of each Symbol.
  public var canTriggerCompletionEvent: CompletionTriggerMap { .init(g: self) }
}

/// Nulled Events
extension Grammar {
  /// Enables the nulled event trigger for `s`.
  ///
  /// - Precondition: `canTriggerNulledEvent[s]``
  public func enableNulledEvent(_ s: Symbol) {
    _ = std(marpa_g_nulled_symbol_activate(g, s.rawID, 1))
  }
  
  /// Disables the nulled event trigger for `s`.
  ///
  /// - Precondition: `canTriggerNulledEvent[s]``
  public func disableNulledEvent(_ s: Symbol) {
    _ = std(marpa_g_nulled_symbol_activate(g, s.rawID, 0))
  }

  /// A writable mapping from a `Symbol` to whether a nulled event can be
  /// triggered for it.
  public struct NulledTriggerMap {
    let g: Grammar

    /// Accesses the “has nulled event” flag of `s`.
    ///
    /// - Precondition(set): `!self.isPrecomputed`
    public subscript(_ s: Symbol) -> Bool {
      get {
        g.std(marpa_g_symbol_is_nulled_event(g.g, s.rawID)) != 0
      }
      nonmutating set {
        _ = g.std(
          marpa_g_symbol_is_nulled_event_set(g.g, s.rawID, newValue ? 1 : 0))
      }
    }
  }

  /// The nulled event trigger capability of each Symbol.
  public var canTriggerNulledEvent: NulledTriggerMap { .init(g: self) }
}

/// Prediction Events
extension Grammar {
  /// Enables the prediction event trigger for `s`.
  ///
  /// - Precondition: `canTriggerPredictionEvent[s]``
  public func enablePredictionEvent(_ s: Symbol) {
    _ = std(marpa_g_prediction_symbol_activate(g, s.rawID, 1))
  }
  
  /// Disables the prediction event trigger for `s`.
  ///
  /// - Precondition: `canTriggerPredictionEvent[s]``
  public func disablePredictionEvent(_ s: Symbol) {
    _ = std(marpa_g_prediction_symbol_activate(g, s.rawID, 0))
  }

  /// A writable mapping from a `Symbol` to whether a prediction event can be
  /// triggered for it.
  public struct PredictionTriggerMap {
    let g: Grammar

    /// Accesses the “has prediction event” flag of `s`.
    ///
    /// - Precondition(set): `!self.isPrecomputed`
    public subscript(_ s: Symbol) -> Bool {
      get {
        g.std(marpa_g_symbol_is_prediction_event(g.g, s.rawID)) != 0
      }
      nonmutating set {
        _ = g.std(
          marpa_g_symbol_is_prediction_event_set(g.g, s.rawID, newValue ? 1 : 0))
      }
    }
  }

  /// The prediction event trigger capability of each Symbol.
  public var canTriggerPredictionEvent: PredictionTriggerMap { .init(g: self) }
}

extension Grammar {
  /// Perform the step necessary to create a recognizer from the grammar.
  public func precompute() {
    _ = std(marpa_g_precompute(g))
  }

  /// `true` iff `precompute()` has been called.
  var isPrecomputed: Bool {
    std(marpa_g_is_precomputed(g)) != 0
  }

  /// `true` iff `self` has a cycle.
  ///
  /// Most applications will want to treat a `true` value as a fatal error. To
  /// determine which rules are in the cycle, `isLoop` can be called for each
  /// rule in turn.
  var hasCycle: Bool {
    std(marpa_g_has_cycle(g)) != 0
  }
}

class Recognizer {
  let g: Grammar;
  let r: Marpa_Recognizer;

  func std(_ i: Int32) -> Int32 {
    if i < 0 { fatal(i) }
    return i
  }
  
  public init(_ g: Grammar) {
    self.g = g
    r = marpa_r_new(g.g)
  }

  deinit {
    marpa_r_unref(r)
  }

  // Makes `self` ready to accept input.
  //
  // The first Earley set, the one at earleme 0, will be completed during this
  // call, so events may be generated. For details, see the documentation of
  // `closeCurrentEarleme`.
  func startInput() {
    _ = std(marpa_r_start_input(r))
  }

  // Reads an input symbol `s` starting at the current earleme, returning a
  // non-nil Marpa error code if the token is not recognized.
  //
  // - Parameter lengthInEarlemes: the number of earlemes spanned by `s` .
  // - Parameter value: a value passed through to the evaluator.
  //
  // - Precondition: `value != 0`
  func read(
    _ s: Grammar.Nonterminal, lengthInEarlemes: Int32 = 1, value: Int32 = 1
  ) -> Int32? {
    let err = marpa_r_alternative(r, s.rawID, value, lengthInEarlemes)
    if err == MARPA_ERR_UNEXPECTED_TOKEN_ID
         || err == MARPA_ERR_DUPLICATE_TOKEN
         || err == MARPA_ERR_NO_TOKEN_EXPECTED_HERE
         || err == MARPA_ERR_INACCESSIBLE_TOKEN { return err }
    if err != MARPA_ERR_NONE { _ = std(err) }
    return nil
  }

  // Advances the current earleme by 1, returning the number of events
  // generated.
  //
  // During this method, one or more events may occur. On success, this function
  // returns the number of events generated, but it is important to note that
  // events may be created whether earleme completion fails or succeeds. When
  // this method fails, the application must call marpa_g_event() if it wants to
  // determine if any events occurred. Since the reason for failure to complete
  // an earleme is often detailed in the events, applications that fail will
  // often be at least as interested in the events as those that succeed.
  // 
  // The MARPA_EVENT_EARLEY_ITEM_THRESHOLD event indicates that an
  // application-settable threshold on the number of Earley items has been reached
  // or exceeded. What this means depends on the application, but when the default
  // threshold is exceeded, it means that it is very likely that the time and
  // space resources consumed by the parse will prove excessive.
  // 
  // A parse is “exhausted” when it can accept no more input. This can happen both
  // on success and on failure. Note that the failure due to parse exhaustion only
  // means failure at the current earleme. There may be successful parses at
  // earlier earlemes.
  // 
  // If a parse is exhausted, but successful, an event with the event code
  // MARPA_EVENT_EXHAUSTED occurs. Because the parse is exhausted, no input will
  // be accepted at later earlemes. It is quite common for a parse to become
  // exhausted when it succeeds. Many practical grammars are designed so that a
  // successful parse cannot be extended.
  // 
  // An exhausted parse may cause a failure, in which case
  // marpa_r_earleme_complete() returns an error whose error code is
  // MARPA_ERR_PARSE_EXHAUSTED. For a parse to fail at an earleme due to
  // exhaustion, it must be the case that no alternatives were accepted at that
  // earleme. In fact, in the standard input model, a failure due to parse
  // exhaustion occurs if and only if no alternatives were accepted at the current
  // earleme.
  // 
  // The circumstances under which failure due to parse exhaustion occurs are
  // slightly more complicated when variable length tokens are in use. Informally,
  // a parse will never fail due to exhaustion as long as it is possible that a
  // token ending at some future earleme will continue the parse. More precisely,
  // a call to marpa_r_earleme_complete() fails due to parse exhaustion if and
  // only if, first, no alternatives were added at the current earleme and,
  // second, that call left the current earleme equal to the furthest earleme.
  @discardableResult
  func advanceEarleme() -> Int {
    Int(std(marpa_r_earleme_complete(r)))
  }
}

let errorDescription: [Int32: StaticString] = [
  MARPA_ERR_NONE: "No error",
  MARPA_ERR_AHFA_IX_NEGATIVE: "MARPA_ERR_AHFA_IX_NEGATIVE",
  MARPA_ERR_AHFA_IX_OOB: "MARPA_ERR_AHFA_IX_OOB",
  MARPA_ERR_ANDID_NEGATIVE: "MARPA_ERR_ANDID_NEGATIVE",
  MARPA_ERR_ANDID_NOT_IN_OR: "MARPA_ERR_ANDID_NOT_IN_OR",
  MARPA_ERR_ANDIX_NEGATIVE: "MARPA_ERR_ANDIX_NEGATIVE",
  MARPA_ERR_BAD_SEPARATOR: "Separator has invalid symbol ID",
  MARPA_ERR_BOCAGE_ITERATION_EXHAUSTED: "MARPA_ERR_BOCAGE_ITERATION_EXHAUSTED",
  MARPA_ERR_COUNTED_NULLABLE: "Nullable symbol on RHS of a sequence rule",
  MARPA_ERR_DEVELOPMENT: "Development error, see string",
  MARPA_ERR_DUPLICATE_AND_NODE: "MARPA_ERR_DUPLICATE_AND_NODE",
  MARPA_ERR_DUPLICATE_RULE: "Duplicate rule",
  MARPA_ERR_DUPLICATE_TOKEN: "Duplicate token",
  MARPA_ERR_YIM_COUNT: "Maximum number of Earley items exceeded",
  MARPA_ERR_YIM_ID_INVALID: "MARPA_ERR_YIM_ID_INVALID",
  MARPA_ERR_EVENT_IX_NEGATIVE: "Negative event index",
  MARPA_ERR_EVENT_IX_OOB: "No event at that index",
  MARPA_ERR_GRAMMAR_HAS_CYCLE: "Grammar has cycle",
  MARPA_ERR_INACCESSIBLE_TOKEN: "Token symbol is inaccessible",
  MARPA_ERR_INTERNAL: "MARPA_ERR_INTERNAL",
  MARPA_ERR_INVALID_AHFA_ID: "MARPA_ERR_INVALID_AHFA_ID",
  MARPA_ERR_INVALID_AIMID: "MARPA_ERR_INVALID_AIMID",
  MARPA_ERR_INVALID_BOOLEAN: "Argument is not boolean",
  MARPA_ERR_INVALID_IRLID: "MARPA_ERR_INVALID_IRLID",
  MARPA_ERR_INVALID_NSYID: "MARPA_ERR_INVALID_NSYID",
  MARPA_ERR_INVALID_LOCATION: "Location is not valid",
  MARPA_ERR_INVALID_RULE_ID: "Rule ID is malformed",
  MARPA_ERR_INVALID_START_SYMBOL: "Specified start symbol is not valid",
  MARPA_ERR_INVALID_SYMBOL_ID: "Symbol ID is malformed",
  MARPA_ERR_I_AM_NOT_OK: "Marpa is in a not OK state",
  MARPA_ERR_MAJOR_VERSION_MISMATCH: "Libmarpa major version number is a mismatch",
  MARPA_ERR_MICRO_VERSION_MISMATCH: "Libmarpa micro version number is a mismatch",
  MARPA_ERR_MINOR_VERSION_MISMATCH: "Libmarpa minor version number is a mismatch",
  MARPA_ERR_NOOKID_NEGATIVE: "MARPA_ERR_NOOKID_NEGATIVE",
  MARPA_ERR_NOT_PRECOMPUTED: "This grammar is not precomputed",
  MARPA_ERR_NOT_TRACING_COMPLETION_LINKS: "MARPA_ERR_NOT_TRACING_COMPLETION_LINKS",
  MARPA_ERR_NOT_TRACING_LEO_LINKS: "MARPA_ERR_NOT_TRACING_LEO_LINKS",
  MARPA_ERR_NOT_TRACING_TOKEN_LINKS: "MARPA_ERR_NOT_TRACING_TOKEN_LINKS",
  MARPA_ERR_NO_AND_NODES: "MARPA_ERR_NO_AND_NODES",
  MARPA_ERR_NO_EARLEY_SET_AT_LOCATION: "Earley set ID is after latest Earley set",
  MARPA_ERR_NO_OR_NODES: "MARPA_ERR_NO_OR_NODES",
  MARPA_ERR_NO_PARSE: "No parse",
  MARPA_ERR_NO_RULES: "This grammar does not have any rules",
  MARPA_ERR_NO_START_SYMBOL: "This grammar has no start symbol",
  MARPA_ERR_NO_TOKEN_EXPECTED_HERE: "No token is expected at this earleme location",
  MARPA_ERR_NO_TRACE_YIM: "MARPA_ERR_NO_TRACE_YIM",
  MARPA_ERR_NO_TRACE_YS: "MARPA_ERR_NO_TRACE_YS",
  MARPA_ERR_NO_TRACE_PIM: "MARPA_ERR_NO_TRACE_PIM",
  MARPA_ERR_NO_TRACE_SRCL: "MARPA_ERR_NO_TRACE_SRCL",
  MARPA_ERR_NULLING_TERMINAL: "A symbol is both terminal and nulling",
  MARPA_ERR_ORDER_FROZEN: "The ordering is frozen",
  MARPA_ERR_ORID_NEGATIVE: "MARPA_ERR_ORID_NEGATIVE",
  MARPA_ERR_OR_ALREADY_ORDERED: "MARPA_ERR_OR_ALREADY_ORDERED",
  MARPA_ERR_PARSE_EXHAUSTED: "The parse is exhausted",
  MARPA_ERR_PARSE_TOO_LONG: "This input would make the parse too long",
  MARPA_ERR_PIM_IS_NOT_LIM: "MARPA_ERR_PIM_IS_NOT_LIM",
  MARPA_ERR_POINTER_ARG_NULL: "An argument is null when it should not be",
  MARPA_ERR_PRECOMPUTED: "This grammar is precomputed",
  MARPA_ERR_PROGRESS_REPORT_EXHAUSTED: "The progress report is exhausted",
  MARPA_ERR_PROGRESS_REPORT_NOT_STARTED: "No progress report has been started",
  MARPA_ERR_RECCE_NOT_ACCEPTING_INPUT: "The recognizer is not accepting input",
  MARPA_ERR_RECCE_NOT_STARTED: "The recognizer has not been started",
  MARPA_ERR_RECCE_STARTED: "The recognizer has been started",
  MARPA_ERR_RHS_IX_NEGATIVE: "RHS index cannot be negative",
  MARPA_ERR_RHS_IX_OOB: "RHS index must be less than rule length",
  MARPA_ERR_RHS_TOO_LONG: "The RHS is too long",
  MARPA_ERR_SEQUENCE_LHS_NOT_UNIQUE: "LHS of sequence rule would not be unique",
  MARPA_ERR_SOURCE_TYPE_IS_AMBIGUOUS: "MARPA_ERR_SOURCE_TYPE_IS_AMBIGUOUS",
  MARPA_ERR_SOURCE_TYPE_IS_COMPLETION: "MARPA_ERR_SOURCE_TYPE_IS_COMPLETION",
  MARPA_ERR_SOURCE_TYPE_IS_LEO: "MARPA_ERR_SOURCE_TYPE_IS_LEO",
  MARPA_ERR_SOURCE_TYPE_IS_NONE: "MARPA_ERR_SOURCE_TYPE_IS_NONE",
  MARPA_ERR_SOURCE_TYPE_IS_TOKEN: "MARPA_ERR_SOURCE_TYPE_IS_TOKEN",
  MARPA_ERR_SOURCE_TYPE_IS_UNKNOWN: "MARPA_ERR_SOURCE_TYPE_IS_UNKNOWN",
  MARPA_ERR_START_NOT_LHS: "Start symbol not on LHS of any rule",
  MARPA_ERR_SYMBOL_VALUED_CONFLICT: "Symbol is treated both as valued and unvalued",
  MARPA_ERR_TERMINAL_IS_LOCKED: "The terminal status of the symbol is locked",
  MARPA_ERR_TOKEN_IS_NOT_TERMINAL: "Token symbol must be a terminal",
  MARPA_ERR_TOKEN_LENGTH_LE_ZERO: "Token length must greater than zero",
  MARPA_ERR_TOKEN_TOO_LONG: "Token is too long",
  MARPA_ERR_TREE_EXHAUSTED: "Tree iterator is exhausted",
  MARPA_ERR_TREE_PAUSED: "Tree iterator is paused",
  MARPA_ERR_UNEXPECTED_TOKEN_ID: "Unexpected token",
  MARPA_ERR_UNPRODUCTIVE_START: "Unproductive start symbol",
  MARPA_ERR_VALUATOR_INACTIVE: "Valuator inactive",
  MARPA_ERR_VALUED_IS_LOCKED: "The valued status of the symbol is locked",
  MARPA_ERR_RANK_TOO_LOW: "Rule or symbol rank too low",
  MARPA_ERR_RANK_TOO_HIGH: "Rule or symbol rank too high",
  MARPA_ERR_SYMBOL_IS_NULLING: "Symbol is nulling",
  MARPA_ERR_SYMBOL_IS_UNUSED: "Symbol is not used",
  MARPA_ERR_NO_SUCH_RULE_ID: "No rule with this ID exists",
  MARPA_ERR_NO_SUCH_SYMBOL_ID: "No symbol with this ID exists",
  MARPA_ERR_BEFORE_FIRST_TREE: "Tree iterator is before first tree",
  MARPA_ERR_SYMBOL_IS_NOT_COMPLETION_EVENT: "Symbol is not set up for completion events",
  MARPA_ERR_SYMBOL_IS_NOT_NULLED_EVENT: "Symbol is not set up for nulled events",
  MARPA_ERR_SYMBOL_IS_NOT_PREDICTION_EVENT: "Symbol is not set up for prediction events",
  MARPA_ERR_RECCE_IS_INCONSISTENT: "MARPA_ERR_RECCE_IS_INCONSISTENT",
  MARPA_ERR_INVALID_ASSERTION_ID: "Assertion ID is malformed",
  MARPA_ERR_NO_SUCH_ASSERTION_ID: "No assertion with this ID exists",
  MARPA_ERR_HEADERS_DO_NOT_MATCH: "Internal error: Libmarpa was built incorrectly",
  MARPA_ERR_NOT_A_SEQUENCE: "Rule is not a sequence"
]
