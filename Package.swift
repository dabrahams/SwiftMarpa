// swift-tools-version: 5.6
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "SwiftMarpa",
    products: [
        .library(
            name: "SwiftMarpa",
            targets: ["SwiftMarpa"]
        ),
    ],
    targets: [
        .target(
            name: "SwiftMarpa",
            dependencies: ["libmarpa"]),
        .testTarget(
            name: "SwiftMarpaTests",
            dependencies: ["SwiftMarpa", "libmarpa"]),
        .systemLibrary(
          name: "libmarpa",
          providers: [
            .apt(["libmarpa-r2-perl"]),
            .brew(["libmarpa"])
          ]
        )
    ]
)
