import libmarpa

let initialConfig: Marpa_Config = {
  var c: Marpa_Config = .init()
  marpa_c_init(&c)
  return c
}()

/// A language syntax.
open class Grammar {
  var g: Marpa_Grammar

  /// Creates an empty instance
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

func fatal(_ e: Int32) -> Never {
  fatalError(
    (Error.allCases.contains { x in x.rawValue == e }
       ? "BUG: unhandled supposedly-recoverable Marpa error "
       : "Marpa error: ")
      + "\(e) \(errorDescription[e] ?? "<unknown>")")
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

/// Types whose legal values are represented in libMarpa's C API as nonnegative
/// `int` values.
public protocol Numbered: Hashable, Comparable {
  typealias ID = UInt32

  /// The underlying numeric value of `self`.
  var id: ID { get }
}

extension Numbered {
  /// The C `int` value associated with `self`.
  var rawID: Int32 { .init(truncatingIfNeeded: id) }

  /// Returns `true` iff the `id` of lhs and rhs are in increasing order.
  public static func <(lhs: Self, rhs: Self) -> Bool {
    return lhs.rawID < rhs.rawID
  }
}

/// A grammar symbol
public struct Symbol: Numbered { public let id: ID }

/// Symbols
extension Grammar {
  /// Returns a new symbol in this grammar.
  func makeSymbol(isTerminal: Bool) -> Symbol {
    let rawID = marpa_g_symbol_new(g)
    let r = std(rawID)
    _ = std(marpa_g_symbol_is_terminal_set(g, rawID, isTerminal ? 1 : 0))
    return Symbol(id: r)
  }

  /// Returns a new terminal symbol in this grammar.
  public func makeTerminal() -> Symbol {
    makeSymbol(isTerminal: true)
  }

  /// Returns a new terminal symbol in this grammar.
  public func makeNonterminal() -> Symbol {
    makeSymbol(isTerminal: false)
  }

  /// Returns true iff `s` is a terminal symbol.
  public func isTerminal(_ s: Symbol) -> Bool {
    std(marpa_g_symbol_is_terminal(g, s.rawID)) != 0
  }

  /// The start symbol, or `nil` if none has been set.
  public var startSymbol: Symbol? {
    get {
      guard let r = stdOpt(marpa_g_start_symbol(g)) else { return nil }
      return Symbol(id: r)
    }
    set {
      _ = std(marpa_g_start_symbol_set(g, newValue?.rawID ?? -2))
    }
  }

  /// The number of symbols in `self`.
  ///
  /// Symbol IDs are in `0..<symbolCount`.
  internal var symbolCount: Int {
    Int(stdOpt(marpa_g_highest_symbol_id(g)) ?? 0) + 1
  }

  public var symbols: LazyMapSequence<Range<Int>, Symbol> {
    (0..<symbolCount).lazy.map { Symbol(id: .init(truncatingIfNeeded: $0)) }
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

/// A BNF parse rule.
public struct Rule: Numbered { public let id: ID }

extension Grammar {
  /// The number of rules in `self`.
  ///
  /// Rule IDs are in `0..<ruleCount`.
  var ruleCount: Int {
    Int(stdOpt(marpa_g_highest_rule_id(g)) ?? 0) + 1
  }

  /// The rules in `self`
  public var rules: LazyMapSequence<Range<Int>, Rule> {
    (0..<ruleCount).lazy.map { Rule(id: .init(truncatingIfNeeded: $0)) }
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
  public func lhs(_ r: Rule) -> Symbol {
    Symbol(id: std(marpa_g_rule_lhs(g, r.rawID)))
  }

  /// Returns a new rule in `self` reducing the symbols in `rhs` to `lhs`.
  public func makeRule<RHS: Collection>(lhs: Symbol, rhs: RHS) -> Rule
    where RHS.Element == Symbol
  {
    var rhsIDs = rhs.map(\.rawID)
    return Rule(
      id: std(marpa_g_rule_new(g, lhs.rawID, &rhsIDs, Int32(rhsIDs.count))))
  }

  /// Returns the `r`'s RHS symbol IDs.
  public func rhs(_ r: Rule) -> LazyMapSequence<Range<Int>, Symbol> {
    (0..<rhsCount(r)).lazy.map {
      .init(id: self.std(marpa_g_rule_rhs(self.g, r.rawID, Int32($0))))
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
    lhs: Symbol, rhs: Symbol.ID, nullable: Bool,
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
extension Rule {
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
  public var rank: RankMap { .init(g: self) }

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
  public var nullRanksHigh: RanksHighMap { .init(g: self) }
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
  /// Perform the step necessary to create a recognizer from the grammar, returning a
  /// non-nil Marpa error code if the grammar contains an error.
  public func precompute() -> Error? {
    return marpa_g_precompute(g) == -2 ? Error(rawValue: err) : nil
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

/// A set of partial parses.  Also a token boundary.
public struct EarleySet: Numbered {
  public init(id: ID) { self.id = id }
  public let id: ID
}
public typealias TokenBoundary = EarleySet

/// An input position.
public struct Earleme: Numbered {
  public init(id: ID) { self.id = id }
  public let id: ID
}
public typealias InputPosition = Earleme

extension EarleySet {
  /// User-defined data associated with each Earley set.
  public typealias Value = (Int32, UnsafeMutableRawPointer?)
}

/// A recognition pass over a given set of tokens, resulting in a `Bocage` (parse forest).
public final class Recognizer {
  let g: Grammar
  let r: Marpa_Recognizer
  weak var progress: ProgressReport?
  
  func std(_ i: Int32) -> UInt32 {
    g.std(i)
  }

  /// Creates an instance recognizing `g`.
  public init(_ g: Grammar) {
    self.g = g
    r = marpa_r_new(g.g)
  }

  deinit {
    marpa_r_unref(r)
  }

  /// Makes `self` ready to accept input.
  ///
  /// The first Earley set, the one at earleme 0, will be completed during this
  /// call, so events may be generated. For details, see the documentation of
  /// `closeCurrentEarleme`.
  public func startInput() {
    _ = std(marpa_r_start_input(r))
  }

  /// Reads an input symbol `s` starting at the current earleme, returning a
  /// non-nil Marpa error code if the token is not recognized.
  ///
  /// - Parameter lengthInEarlemes: the number of earlemes spanned by `s` .
  /// - Parameter value: a value passed through to the evaluator.
  ///
  /// - Precondition: `value != 0`
  public func read(
    _ s: Symbol, lengthInEarlemes: Int32 = 1, value: Int32 = 1
  ) -> Error? {
    let err = marpa_r_alternative(r, s.rawID, value, lengthInEarlemes)
    if err == MARPA_ERR_NONE { return nil }
    if err == MARPA_ERR_UNEXPECTED_TOKEN_ID
         || err == MARPA_ERR_DUPLICATE_TOKEN
         || err == MARPA_ERR_NO_TOKEN_EXPECTED_HERE
         || err == MARPA_ERR_INACCESSIBLE_TOKEN { return Error(rawValue: err) }
    _ = std(err)
    return nil
  }

  // Advances the current earleme by 1.
  public func advanceEarleme() {
    let e = marpa_r_earleme_complete(r)
    if g.err != MARPA_ERR_PARSE_EXHAUSTED {
      _ = std(e)
    }
  }

  /// The current earleme, or `nil` if `startInput` has not yet been called.
  public var currentEarleme: Earleme? {
    g.stdOpt(marpa_r_current_earleme(r)).map { .init(id: $0) }
  }

  /// Returns the Earleme corresponding to `s`.
  ///
  /// - Precondition: `s` is a valid earley set in `self`.
  public func earleme(_ s: EarleySet) -> Earleme {
    Earleme(id: std(marpa_r_earleme(r, s.rawID)))
  }
  
  /// Returns the value associated with the given Earley set.
  public func value(for s: EarleySet) -> EarleySet.Value {
    var result: EarleySet.Value = (0, nil)
    _ = std(marpa_r_earley_set_values(r, s.rawID, &result.0, &result.1))
    return result
  }

  /// Returns the value associated with the given Earley set.
  public func setValueForLatestEarleySet(_ v: EarleySet.Value) {
    _ = std(marpa_r_latest_earley_set_values_set(r, v.0, v.1))
  }

  /// The maximal end position of any token so far.
  public var furthestEarleme: Earleme {
    .init(id: marpa_r_furthest_earleme(r))
  }

  /// The last Earley set completed.
  public var latestEarleySet: EarleySet {
    .init(id: std(marpa_r_latest_earley_set(r)))
  }
}

/// Other parse status methods
extension Recognizer {
  /// Enables the completion event trigger for `s`.
  ///
  /// - Precondition: `s` was set up for completion events in the grammar.
  public func enableCompletionEvent(_ s: Symbol) {
    _ = std(marpa_r_completion_symbol_activate(r, s.rawID, 1))
  }
  
  /// Disables the completion event trigger for `s`.
  ///
  /// - Precondition: `s` was set up for completion events in the grammar.
  public func disableCompletionEvent(_ s: Symbol) {
    _ = std(marpa_r_completion_symbol_activate(r, s.rawID, 0))
  }

  /// An Earley set size above which `MARPA_EVENT_EARLEY_ITEM_THRESHOLD` is
  /// triggered.
  ///
  /// If threshold is zero or less, an unlimited number of Earley items will be
  /// allowed without warning. This will rarely be what the user wants.  By
  /// default, a likely-appropriate value is calculated based on the grammar. 
  public var earleyItemWarningThreshold: Int32 {
    get {
      marpa_r_earley_item_warning_threshold(r)
    }
    set {
      marpa_r_earley_item_warning_threshold_set(r, newValue)
    }
  }

  /// Enables the expected symbol event trigger for `s`.
  ///
  /// - Precondition: `s` is neither nulling, inaccessible, nor unproductive.
  public func enableExpectedEvent(_ s: Symbol) {
    _ = std(marpa_r_completion_symbol_activate(r, s.rawID, 1))
  }
  
  /// Disables the expected symbol event trigger for `s`.
  ///
  /// - Precondition: `s` is neither nulling, inaccessible, nor unproductive.
  public func disableExpectedEvent(_ s: Symbol) {
    _ = std(marpa_r_completion_symbol_activate(r, s.rawID, 0))
  }

  /// True iff `self` cannot accept any more input.
  public var isExhausted: Bool {
    marpa_r_is_exhausted(r) != 0
  }

  /// Enables the nulled event trigger for `s`.
  ///
  /// - Precondition: `s` was set up for nulled events in the grammar.
  public func enableNulledEvent(_ s: Symbol) {
    _ = std(marpa_r_nulled_symbol_activate(r, s.rawID, 1))
  }
  
  /// Disables the nulled event trigger for `s`.
  ///
  /// - Precondition: `s` was set up for nulled events in the grammar.
  public func disableNulledEvent(_ s: Symbol) {
    _ = std(marpa_r_nulled_symbol_activate(r, s.rawID, 0))
  }

  /// Enables the symbol prediction event trigger for `s`.
  ///
  /// - Precondition: `s` was set up for prediction events in the grammar.
  public func enablePredictionEvent(_ s: Symbol) {
    _ = std(marpa_r_prediction_symbol_activate(r, s.rawID, 1))
  }
  
  /// Disables the symbol prediction event trigger for `s`.
  ///
  /// - Precondition: `s` was set up for prediction events in the grammar.
  public func disablePredictionEvent(_ s: Symbol) {
    _ = std(marpa_r_prediction_symbol_activate(r, s.rawID, 0))
  }

  /// Returns the symbols acceptable as tokens at the current earleme.
  public var expectedTerminals: [Symbol] {
    var rawIDs: [Marpa_Symbol_ID] = Array(repeating: 0, count: g.symbolCount)
    let n = Int(std(marpa_r_terminals_expected(r, &rawIDs)))
    return rawIDs[..<n].map { Symbol(id: Symbol.ID(truncatingIfNeeded: $0)) }
  }

  /// Returns `true` iff `s` is expected at the current earleme.
  public func expects(_ s: Symbol) -> Bool {
    std(marpa_r_terminal_is_expected(r, s.rawID)) != 0
  }
}

extension Recognizer {
  /// A single-pass sequence whose elements describe each rule being recognized
  /// at a given position, where the rule started, and how many of its RHS
  /// symbols have been recognized.
  public final class ProgressReport: Sequence, IteratorProtocol {
    let r: Recognizer

    init(of r: Recognizer, at position: EarleySet) {
      self.r = r
      _ = r.std(marpa_r_progress_report_start(r.r, position.rawID))
    }

    deinit {
      marpa_r_progress_report_finish(r.r)
    }

    /// Returns the next rule being recognized, where the rule started, and how
    /// many of its RHS symbols have been recognized, or `nil` if there are no
    /// more rules being recognized.
    public func next() -> (Rule, origin: EarleySet, progress: Int)? {
      var progress: Int32 = 0
      var origin: Int32 = 0
      let ruleID = marpa_r_progress_item(r.r, &progress, &origin)
      if ruleID == -1 { return nil }
      let rule = Rule(id: r.std(ruleID))
      return (
        rule,
        origin: .init(id: UInt32(origin)),
        progress: progress < 0 ? r.g.rhs(rule).count : Int(progress))
    }
  }
  
  /// Returns a sequence whose elements describe each rule being recognized at
  /// `position`, where the rule started, and how many of its RHS symbols have
  /// been recognized.
  ///
  /// - Precondition: no other progress report for `self` exists.
  public func progress(at position: EarleySet) -> ProgressReport {
    let progress = ProgressReport(of: self, at: position)
    self.progress = progress
    return progress
  }
}

/// The parse forest for a given recognition pass.
public final class Bocage {
  let b: Marpa_Bocage
  let g: Grammar

  /// Creates an instance ending at `endPosition`, or at `r`'s current earley
  /// set if `endPosition` is `nil`, yielding `nil` if there are no such valid
  /// parses.
  public init?(_ r: Recognizer, endPosition: EarleySet? = nil) {
    g = r.g
    guard let b = marpa_b_new(r.r, endPosition?.rawID ?? -1) else {
      if g.err == MARPA_ERR_NO_PARSE { return nil }
      fatal(g.err)
    }
    self.b = b
  }

  /// `True` iff the parse set was ambiguous.
  public var isAmbiguous: Bool {
    g.std(marpa_b_ambiguity_metric(b)) > 1
  }
  
  /// `True` iff the parsed input was empty.
  public var isNull: Bool {
    g.std(marpa_b_is_null(b)) != 0
  }
  
  deinit {
    marpa_b_unref(b)
  }
}

/// Transliterations of macros in the Marpa C API.
func marpa_v_step_type(_ v: Marpa_Value) -> Int32 { v[0].t_step_type }
func marpa_v_token(_ v: Marpa_Value) -> Int32 { v[0].t_token_id }
let marpa_v_symbol = marpa_v_token
func marpa_v_token_value(_ v: Marpa_Value) -> Int32 { v[0].t_token_value }
func marpa_v_rule(_ v: Marpa_Value) -> Int32 { v[0].t_rule_id }
func marpa_v_arg_0(_ v: Marpa_Value) -> Int32 { v[0].t_arg_0 }
func marpa_v_arg_n(_ v: Marpa_Value) -> Int32 { v[0].t_arg_n }
func marpa_v_result(_ v: Marpa_Value) -> Int32 { v[0].t_result }
func marpa_v_rule_start_es_id(_ v: Marpa_Value) -> Int32 { v[0].t_rule_start_ys_id }
func marpa_v_token_start_es_id(_ v: Marpa_Value) -> Int32 { v[0].t_token_start_ys_id }
func marpa_v_es_id(_ v: Marpa_Value) -> Int32 { v[0].t_ys_id }

/// A single-pass sequence of bottom-up semantic evaluation steps expressed in
/// terms of a stack of semantic values.
public final class Evaluation: Sequence, IteratorProtocol {
  let v: Marpa_Value
  let g: Grammar

  /// A semantic evaluation step.
  public enum Step: Hashable {
    /// Instruction to place the semantic value for the given symbol into the
    /// `output` stack lockation.
    ///
    /// - `tokenValue`: if the symbol is a token, the `value` passed to its
    ///   `Recognizer.read` call, and `nil` otherwise (if
    ///   `sourceRange.isEmpty`).  - `sourceRange`: the range of token
    ///   boundaries covered by the recognized symbol.
    case symbol(Symbol, output: UInt32, tokenValue: Int32?, sourceRange: Range<EarleySet>)

    /// Instruction to replace the first `input` stack location with the
    /// semantic value of the given rule's LHS as computed from its RHS values
    /// in the `input` stack locations.
    case rule(Rule, input: ClosedRange<UInt32>, sourceRange: Range<EarleySet>)

    /// The stack location where the step's result should be stored.
    public var output: UInt32 {
      switch self {
      case .symbol(_, let r, _, _): return r
      case .rule(_, let r, _): return r.first!
      }
    }

    /// The range of source locations covered by the input being evaluated.
    public var sourceRange: Range<EarleySet> {
      switch self {
      case .symbol(_, _, _, let r), .rule(_, _, let r):
        return r
      }
    }

    /// The associated values of `self` if it is `.symbol(...)`; nil otherwise.
    public var symbol: (Symbol, output: UInt32, tokenValue: Int32?, sourceRange: Range<EarleySet>)?
    {
      if case let .symbol(s, l, t, r) = self { return (s, l, t, r) }
      return nil
    }

    /// The associated values of `self` if it is `.rule(...)`; nil otherwise.
    public var rule: (Rule, input: ClosedRange<UInt32>, sourceRange: Range<EarleySet>)?
    {
      if case let .rule(s, l, r) = self { return (s, l, r) }
      return nil
    }
  }

  /// Returns the next evaluation `Step`, or nil if all steps are exhausted.
  public func next() -> Step? {
    let r = marpa_v_step(v)
    if r == MARPA_STEP_INACTIVE { return nil }
    let sourceEnd = EarleySet(id: .init(marpa_v_es_id(v)))
    
    switch r {
    case MARPA_STEP_RULE:
      return .rule(
        Rule(id: Rule.ID(marpa_v_rule(v))),
        input: UInt32(marpa_v_arg_0(v))...UInt32(marpa_v_arg_n(v)),
        sourceRange:
          EarleySet(id: .init(marpa_v_rule_start_es_id(v)))..<sourceEnd
      )
    case MARPA_STEP_TOKEN, MARPA_STEP_NULLING_SYMBOL:
      return .symbol(
        Symbol(id: Symbol.ID(marpa_v_token(v))),
        output: UInt32(marpa_v_result(v)),
        tokenValue: r == MARPA_STEP_TOKEN ? marpa_v_token_value(v) : nil,
        sourceRange:
          EarleySet(id: .init(marpa_v_token_start_es_id(v)))..<sourceEnd
      )
    default:
      fatal(g.err)
    }
  }

  /// Creates an instance for the given parse tree and grammar.
  init(_ t: Marpa_Tree, _ g: Grammar) {
    v = marpa_v_new(t)
    self.g = g
  }
  
  deinit {
    marpa_v_unref(v)
  }
}

/// A sequence of semantic evaluation sequences, each for a different successful
/// parse in a `Bocage` (forest).
public final class Order: Sequence {
  let o: Marpa_Order
  let g: Grammar

  /// A traversal over a sequence of semantic evaluation sequences, each for a
  /// different successful parse in a `Bocage` (forest).
  public final class Iterator: IteratorProtocol {
    let t: Marpa_Tree
    let g: Grammar

    init(_ o: Order) {
      g = o.g
      guard let t = marpa_t_new(o.o) else { fatal(g.err) }
      self.t = t
    }

    /// Returns the evaluation sequence for the next parse tree, or `nil` if
    /// successful parses are exhausted.
    public func next() -> Evaluation? {
      let e = marpa_t_next(t)
      if e == -1 { return nil }
      _ = g.std(e)
      return Evaluation(t, g)
    }

    deinit {
      marpa_t_unref(t)
    }
  }

  /// Returns a traversal over `self`.
  public func makeIterator() -> Iterator {
    return Iterator(self)
  }

  /// Creates an instance for the highest-ranking successful parses in `b`, or
  /// iff highRankOnly is `false`, all of the successful parses.
  public init(_ b: Bocage, highRankOnly: Bool = true) {
    g = b.g
    guard let o = marpa_o_new(b.b) else { fatal(g.err) }
    self.o = o
    _ = g.std(marpa_o_high_rank_only_set(o, highRankOnly ? 1 : 0))
    _ = g.std(marpa_o_rank(o))
  }

  /// True iff self was initialized with `highRankOnly: true`.
  public var containsHighRankTreesOnly: Bool {
    g.std(marpa_o_high_rank_only(o)) != 0
  }
  
  /// True iff the parse set was ambiguous.
  ///
  /// - See also: https://github.com/jeffreykegler/libmarpa/issues/112).
  public var isAmbiguous: Bool {
    g.std(marpa_o_ambiguity_metric(o)) > 1
  }
  
  /// True iff the parsed input was empty.
  public var isNull: Bool {
    return g.std(marpa_o_is_null(o)) != 0
  }
  
  deinit {
    marpa_o_unref(o)
  }
}

public enum Event: Hashable {
  /// A nullable symbol appears on the right hand side of a sequence rule.
  ///
  /// The presence of one or more of these will prevent the grammar from being
  /// precomputed.
  case countedNullable(Symbol)

  /// Too many earley items were added: it is very likely that resources consumed
  /// by the parse will prove excessive.
  case earleyItemThresholdExceeded(earleyItemCount: Int32)

  /// The parse is complete and no input can be accepted at later token positions.
  case parseExhausted

  /// The given number of loop rules were detected.
  case loopRules(count: Int32)

  /// The given symbol is both a terminal and a nulling symbol, which is not allowed.
  case nullingTerminal(Symbol)

  /// The given symbol was recognized in a partial parse of the input.
  case completed(Symbol)

  /// The given token is expected next in the input by one of the partial parses.
  case expected(Symbol)

  /// The given nonterminal was recognized as empty in a partial parse of the input.
  case nulled(Symbol)

  /// The given nonterminal is expected next in the input by one of the partial parses.
  case predicted(Symbol)
}

extension Grammar {
  /// The events associated with operations on a `Grammar` and its `Recognizer`s.
  public struct Events: RandomAccessCollection {
    let g: Grammar

    /// The position of the first element, or `endIndex` if `self.isEmpty`
    public var startIndex: Int { 0 }
    /// The position one past the last element.
    public var endIndex: Int { Int(marpa_g_event_count(g.g)) }

    /// Accesses the element at `i`.
    public subscript(i: Int) -> Event {
      var e = Marpa_Event(t_type: 0, t_value: 0)
      if marpa_g_event(g.g, &e, Int32(i)) == -2 {
        fatal(g.err)
      }
      switch e.t_type {
      case MARPA_EVENT_COUNTED_NULLABLE:
        return .countedNullable(Symbol(id: Symbol.ID(e.t_value)))
      case MARPA_EVENT_EARLEY_ITEM_THRESHOLD:
        return .earleyItemThresholdExceeded(earleyItemCount: e.t_value)
      case MARPA_EVENT_EXHAUSTED:
        return .parseExhausted
      case MARPA_EVENT_LOOP_RULES:
        return .loopRules(count: e.t_value)
      case MARPA_EVENT_NULLING_TERMINAL:
        return .nullingTerminal(Symbol(id: Symbol.ID(e.t_value)))
      case MARPA_EVENT_SYMBOL_COMPLETED:
        return .completed(Symbol(id: Symbol.ID(e.t_value)))
      case MARPA_EVENT_SYMBOL_EXPECTED:
        return .expected(Symbol(id: Symbol.ID(e.t_value)))
      case MARPA_EVENT_SYMBOL_NULLED:
        return .nulled(Symbol(id: Symbol.ID(e.t_value)))
      case MARPA_EVENT_SYMBOL_PREDICTED:
        return .predicted(Symbol(id: Symbol.ID(e.t_value)))
      default:
        fatal(g.err)
      }
    }
  }

  /// The events associated with operations on `self` and any `Recognizers` of `self`.
  public var events: Events {
    return Events(g: self)
  }
}

public let errorDescription: [Int32: StaticString] = [
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

public enum Error: Int32, CaseIterable, Swift.Error, CustomStringConvertible {
  case bocageIterationExhausted = 7      // MARPA_ERR_BOCAGE_ITERATION_EXHAUSTED
  case grammarHasCycle = 17              // MARPA_ERR_GRAMMAR_HAS_CYCLE
  case duplicateRule = 11                // MARPA_ERR_DUPLICATE_RULE
  case duplicateToken = 12               // MARPA_ERR_DUPLICATE_TOKEN
  case inaccessibleToken = 18            // MARPA_ERR_INACCESSIBLE_TOKEN
  case notPrecomputed = 34               // MARPA_ERR_NOT_PRECOMPUTED
  case noParse = 41                      // MARPA_ERR_NO_PARSE
  case noRules = 42                      // MARPA_ERR_NO_RULES
  case noStartSymbol = 43                // MARPA_ERR_NO_START_SYMBOL
  case noTokenExpectedHere = 44          // MARPA_ERR_NO_TOKEN_EXPECTED_HERE
  case parseExhausted = 53               // MARPA_ERR_PARSE_EXHAUSTED
  case progressReportExhausted = 58      // MARPA_ERR_PROGRESS_REPORT_EXHAUSTED
  case startNotLHS = 73                  // MARPA_ERR_START_NOT_LHS
  case treeExhausted = 79                // MARPA_ERR_TREE_EXHAUSTED
  case unexpectedToken = 81              // MARPA_ERR_UNEXPECTED_TOKEN_ID
  case unproductiveStart = 82            // MARPA_ERR_UNPRODUCTIVE_START
  case recceIsInconsistent = 95          // MARPA_ERR_RECCE_IS_INCONSISTENT

  public var description: String { "\(errorDescription[rawValue]!)" }
}
