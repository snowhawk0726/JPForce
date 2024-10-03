//
//  SymbolTableTests.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/09/30.
//

import XCTest

final class SymbolTableTests : XCTestCase {
    override func setUpWithError() throws {
    }
    override func tearDownWithError() throws {
    }
    func testDefine() throws {
        let expected = [
            "a": Symbol(name: "a", scope: .GLOBAL, index: 0),
            "b": Symbol(name: "b", scope: .GLOBAL, index: 1),
        ]
        let global = SymbolTable()
        XCTAssertEqual(global.define("a"), expected["a"])
        XCTAssertEqual(global.define("b"), expected["b"])
    }
    func testResolveGlobal() throws {
        let global = SymbolTable()
        _ = global.define("a")
        _ = global.define("b")
        let expected = [
            "a": Symbol(name: "a", scope: .GLOBAL, index: 0),
            "b": Symbol(name: "b", scope: .GLOBAL, index: 1),
        ]
        for (name, symbol) in expected {
            if let result = global.resolve(name) {
                XCTAssertEqual(result, symbol, "expected \(name) to resolve to \(symbol), got=\(result)")
            } else {
                XCTFail("expected \(name) to resolve to \(symbol), but it was not found.")
            }
        }
    }
}
