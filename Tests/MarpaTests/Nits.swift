import XCTest
import Marpa

/// Test case mirroring nits.c from the libMarpa repo.
final class Nits: XCTestCase {
  func test() {
    let g = TestGrammar(nullableCs: false)
    // precompute
    g.g.startSymbol = g.top
    g.g.precompute()

    var r = Recognizer(g.g)
    r.startInput()
    r.enableExpectedEvent(g.a2)
    r.advanceEarleme()
    XCTAssert(r.isExhausted)

    r = Recognizer(g.g)
    r.startInput()
    let err = r.read(g.c1, value: 1)
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
    XCTAssert(o.containsHighRankTreesOnly)
  }
}

