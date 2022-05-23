import Marpa

@dynamicMemberLookup
class TestGrammar: Marpa.Grammar {
  struct Parts {
    let top, a1, a2, b1, b2, c1, c2: Symbol
    let top1, top2: Rule
    let c2_1: Rule!
  }

  private var parts: Parts?

  subscript<T>(dynamicMember m: KeyPath<Parts, T>) -> T { parts![keyPath: m] }

  init(nullableCs: Bool) {
    super.init()
    let top = makeNonterminal()
    let a1 = makeNonterminal()
    let a2 = makeNonterminal()
    let b1 = makeNonterminal()
    let b2 = makeNonterminal()
    let c1 = nullableCs ? makeNonterminal() : makeTerminal()
    let c2 = nullableCs ? makeNonterminal() : makeTerminal()
    
    let top1 = makeRule(lhs: top, rhs: [a1])
    let top2 = makeRule(lhs: top, rhs: [a2])
    _ = makeRule(lhs: a1, rhs: [b1])
    _ = makeRule(lhs: a2, rhs: [b2])
    _ = makeRule(lhs: b1, rhs: [c1])
    _ = makeRule(lhs: b2, rhs: [c2])
    
    _ = nullableCs ? makeRule(lhs: c1, rhs: []) : nil
    let c2_1 = nullableCs ? makeRule(lhs: c2, rhs: []) : nil
    parts = Parts(
      top: top, a1: a1, a2: a2, b1: b1, b2: b2, c1: c1, c2: c2, top1: top1, top2: top2, c2_1: c2_1)
  }
}
