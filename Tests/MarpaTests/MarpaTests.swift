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

  func testSmoke() {
    do {
      let g = Grammar()
      _ = g
    }
  }

  func testEarleySetAndEarlemeConstruction() {
    let s = EarleySet(id: 0)
    XCTAssertEqual(s.id, 0)
    let t = Earleme(id: 4)
    XCTAssertEqual(t.id, 4)
  }
}

