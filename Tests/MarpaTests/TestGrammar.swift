import Marpa

struct TestGrammar {
  private(set) var g = Marpa.Grammar()
  let top, a1, a2, b1, b2, c1, c2: Symbol
  let top1, top2: Rule
  var c2_1: Rule? = nil
  
  init(nullableCs: Bool) {
    top = g.makeNonterminal()
    a1 = g.makeNonterminal()
    a2 = g.makeNonterminal()
    b1 = g.makeNonterminal()
    b2 = g.makeNonterminal()
    c1 = nullableCs ? g.makeNonterminal() : g.makeTerminal()
    c2 = nullableCs ? g.makeNonterminal() : g.makeTerminal()
    
    top1 = g.makeRule(lhs: top, rhs: [a1])
    top2 = g.makeRule(lhs: top, rhs: [a2])
    _ = g.makeRule(lhs: a1, rhs: [b1])
    _ = g.makeRule(lhs: a2, rhs: [b2])
    _ = g.makeRule(lhs: b1, rhs: [c1])
    _ = g.makeRule(lhs: b2, rhs: [c2])
    
    _ = nullableCs ? g.makeRule(lhs: c1, rhs: []) : nil
    c2_1 = nullableCs ? g.makeRule(lhs: c2, rhs: []) : nil
  }
}
