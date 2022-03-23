import XCTest
//@testable import SwiftMarpa

import libmarpa

final class SwiftMarpaTests: XCTestCase {
  func testExample() throws {
    var c: Marpa_Config = .init()
    marpa_c_init (&c)
    let g = marpa_g_new(&c)
    print(g!)
  }
}
