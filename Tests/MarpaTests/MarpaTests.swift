import XCTest
import Marpa

import libmarpa

final class SwiftMarpaTests: XCTestCase {
  func testCAPI()  {
    var c: Marpa_Config = .init()
    marpa_c_init (&c)
    let g = marpa_g_new(&c)
    XCTAssertNotNil(g)
  }

  func smoke() {
    do {
      let g = Grammar()
      _ = g
    }
  }
}

/// Test case mirroring nits.c from the libMarpa repo.
final class Nits: XCTestCase {
  struct NitsGrammar {
    let g = Marpa.Grammar()
    let top, a1, a2, b1, b2, c1, c2: Symbol
    let top1, top2: Rule
    
    init() {
      top = g.makeNonterminal()
      a1 = g.makeNonterminal()
      a2 = g.makeNonterminal()
      b1 = g.makeNonterminal()
      b2 = g.makeNonterminal()
      c1 = g.makeTerminal()
      c2 = g.makeTerminal()
      
      top1 = g.makeRule(lhs: top, rhs: [a1])
      top2 = g.makeRule(lhs: top, rhs: [a2])
      _ = g.makeRule(lhs: a1, rhs: [b1])
      _ = g.makeRule(lhs: a2, rhs: [b2])
      _ = g.makeRule(lhs: b1, rhs: [c1])
      _ = g.makeRule(lhs: b2, rhs: [c2])
    }
  }
  
  func test() {
    let nits = NitsGrammar()
    // precompute
    nits.g.startSymbol = nits.top
    nits.g.precompute()

    var r = Recognizer(nits.g)
    r.startInput()
    r.enableExpectedEvent(nits.a2)
    r.advanceEarleme()
    XCTAssert(r.isExhausted)

    r = Recognizer(nits.g)
    r.startInput()
    let err = r.read(nits.c1, value: 1)
    XCTAssertNil(err)
    r.advanceEarleme()
    XCTAssert(r.isExhausted)

    guard let b = Bocage(r) else {
      XCTFail("unexpected nil Bocage")
      return
    }
    XCTAssertFalse(b.isAmbiguous)
    XCTAssertFalse(b.isNull)

    let o = Order(b, highRankOnly: true)
    XCTAssertFalse(o.isAmbiguous)
    XCTAssert(o.highRankOnly)
  }
}

