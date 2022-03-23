// swift-tools-version: 5.6
import PackageDescription

let package = Package(
    name: "Marpa",
    products: [
      .library(name: "Marpa", targets: ["Marpa"]),
    ],
    targets: [
      .target(name: "Marpa", dependencies: ["libmarpa"]),
      .testTarget(
        name: "MarpaTests", dependencies: ["Marpa", "libmarpa"]),
      .systemLibrary(
        name: "libmarpa", pkgConfig: "libmarpa",
        // These provide hints for users about what to install.
        providers: [
          .apt(["libmarpa-r2-perl"]),
          .brew(["libmarpa"]),
          .yum(["perl-Marpa-XS"])
        ])
    ])
