import XCTest
import Marpa

private extension TestGrammar {
  func isNullable(_ s: Symbol) -> Bool {
    return [top, a1, a2, b1, b2, c1, c2].contains(s)
  }
}

/// Test case mirroring trivial.c from the libMarpa repo.
final class Trivial: XCTestCase {
  func test() {
    let g = TestGrammar(nullableCs: true)
    for s in g.g.symbols {
      g.g.canTriggerNulledEvent[s] = true
    }
    g.g.startSymbol = g.top
    g.g.precompute()

    let r = Recognizer(g.g)
    r.startInput()
    XCTAssert(r.isExhausted)

    
    // Inner part
    var nulledEventCount = [Symbol: Int](
      uniqueKeysWithValues: g.g.symbols.lazy.map { ($0, 0) })
    
    var exhaustionEventCount = 0
    XCTAssertEqual(g.g.events.count, 8)
    for e in g.g.events {
      switch e {
      case .nulled(let s): nulledEventCount[s]! += 1
      case .parseExhausted: exhaustionEventCount += 1
      default: XCTFail("Unexpected event \(e)")
      }
    }
    XCTAssertEqual(exhaustionEventCount, 1)

    for s in g.g.symbols {
      XCTAssertEqual(nulledEventCount[s], g.isNullable(s) ? 1 : 0, "\(s)")
    }
  }
}
