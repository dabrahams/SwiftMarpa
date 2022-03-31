import Marpa

struct TestGrammar {
  let g = Marpa.Grammar()
  let top, a1, a2, b1, b2, c1, c2: Symbol
  let top1, top2: Rule
  
  init(terminalCs: Bool = true) {
    top = g.makeNonterminal()
    a1 = g.makeNonterminal()
    a2 = g.makeNonterminal()
    b1 = g.makeNonterminal()
    b2 = g.makeNonterminal()
    if terminalCs {
      c1 = g.makeTerminal()
      c2 = g.makeTerminal()
    }
    else {
      c1 = g.makeNonterminal()
      c2 = g.makeNonterminal()
    }
    
    top1 = g.makeRule(lhs: top, rhs: [a1])
    top2 = g.makeRule(lhs: top, rhs: [a2])
    _ = g.makeRule(lhs: a1, rhs: [b1])
    _ = g.makeRule(lhs: a2, rhs: [b2])
    _ = g.makeRule(lhs: b1, rhs: [c1])
    _ = g.makeRule(lhs: b2, rhs: [c2])
  }

  var allSymbols: [Symbol] {
    return [top, a1, a2, b1, b2, c1, c2]
  }
}

