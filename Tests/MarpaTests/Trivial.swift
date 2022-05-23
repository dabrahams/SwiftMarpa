import XCTest
import Marpa

private extension TestGrammar {
  func knownNullable(_ s: Symbol) -> Bool {
    return [self.top, self.a1, self.a2, self.b1, self.b2, self.c1, self.c2].contains(s)
  }
}

/// Test case mirroring trivial.c from the libMarpa repo.
final class Trivial: XCTestCase {
  func test() {
    let g = TestGrammar(nullableCs: true)
    for s in g.symbols {
      g.canTriggerNulledEvent[s] = true
    }
    g.startSymbol = g.top
    g.precompute()

    let r = Recognizer(g)
    r.startInput()
    XCTAssert(r.isExhausted)

    
    // Inner part
    var nulledEventCount = [Symbol: Int](
      uniqueKeysWithValues: g.symbols.lazy.map { ($0, 0) })
    
    var exhaustionEventCount = 0
    XCTAssertEqual(g.events.count, 8)
    for e in g.events {
      switch e {
      case .nulled(let s): nulledEventCount[s]! += 1
      case .parseExhausted: exhaustionEventCount += 1
      default: XCTFail("Unexpected event \(e)")
      }
    }
    XCTAssertEqual(exhaustionEventCount, 1)

    for s in g.symbols {
      XCTAssertEqual(nulledEventCount[s], g.knownNullable(s) ? 1 : 0, "\(s)")
    }
  }
}
