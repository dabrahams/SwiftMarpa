import XCTest
@testable import Marpa

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
