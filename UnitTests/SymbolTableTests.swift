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
            "c": Symbol(name: "c", scope: .LOCAL, index: 0),
            "d": Symbol(name: "d", scope: .LOCAL, index: 1),
            "e": Symbol(name: "e", scope: .LOCAL, index: 0),
            "f": Symbol(name: "f", scope: .LOCAL, index: 1),
        ]
        let global = SymbolTable()
        let a = global.define("a")
        XCTAssertEqual(a, expected["a"])
        let b = global.define("b")
        XCTAssertEqual(b, expected["b"])
        let firrstLocal = SymbolTable(outer: global)
        let c = firrstLocal.define("c")
        XCTAssertEqual(c, expected["c"])
        let d = firrstLocal.define("d")
        XCTAssertEqual(d, expected["d"])
        let secondLocal = SymbolTable(outer: firrstLocal)
        let e = secondLocal.define("e")
        XCTAssertEqual(e, expected["e"])
        let f = secondLocal.define("f")
        XCTAssertEqual(f, expected["f"])
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
    func testResolveLocal() throws {
        let global = SymbolTable()
        _ = global.define("a")
        _ = global.define("b")
        let local = SymbolTable(outer: global)
        _ = local.define("c")
        _ = local.define("d")
        let expected = [
            Symbol(name: "a", scope: .GLOBAL, index: 0),
            Symbol(name: "b", scope: .GLOBAL, index: 1),
            Symbol(name: "c", scope: .LOCAL, index: 0),
            Symbol(name: "d", scope: .LOCAL, index: 1),
        ]
        for symbol in expected {
            let result = try XCTUnwrap(local.resolve(symbol.name))
            XCTAssertEqual(result, symbol)
        }
    }
    func testResolveNestedLocal() throws {
        let global = SymbolTable()
        _ = global.define("a")
        _ = global.define("b")
        let firstLocal = SymbolTable(outer: global)
        _ = firstLocal.define("c")
        _ = firstLocal.define("d")
        let secondLocal = SymbolTable(outer: firstLocal)
        _ = secondLocal.define("e")
        _ = secondLocal.define("f")
        let testPatterns: [(table: SymbolTable,symbols: [Symbol])] = [
            (firstLocal,
             [
                Symbol(name: "a", scope: .GLOBAL, index: 0),
                Symbol(name: "b", scope: .GLOBAL, index: 1),
                Symbol(name: "c", scope: .LOCAL, index: 0),
                Symbol(name: "d", scope: .LOCAL, index: 1),
             ]),
            (secondLocal,
             [
                Symbol(name: "a", scope: .GLOBAL, index: 0),
                Symbol(name: "b", scope: .GLOBAL, index: 1),
                Symbol(name: "e", scope: .LOCAL, index: 0),
                Symbol(name: "f", scope: .LOCAL, index: 1),
             ]),
        ]
        for test in testPatterns {
            for symbol in test.symbols {
                let result = try XCTUnwrap(test.table.resolve(symbol.name))
                XCTAssertEqual(result, symbol)
            }
        }
    }
}
