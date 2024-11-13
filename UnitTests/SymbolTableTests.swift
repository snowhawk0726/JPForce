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
    func testDefineResolevePredicates() throws {
        let global = SymbolTable()
        let firstLocal = SymbolTable(outer: global)
        let secondLocal = SymbolTable(outer: firstLocal)
        let expected = [
            Symbol(name: "a", scope: .PREDICATE, index: 0),
            Symbol(name: "c", scope: .PREDICATE, index: 1),
            Symbol(name: "e", scope: .PREDICATE, index: 2),
            Symbol(name: "f", scope: .PREDICATE, index: 3),
        ]
        for (index, symbol) in expected.enumerated() {
            _ = global.define(name: symbol.name, index: index, scope: .PREDICATE)
        }
        for table in [global, firstLocal, secondLocal] {
            for symbol in expected {
                let result = try XCTUnwrap(table.resolve(symbol.name))
                XCTAssertEqual(result, symbol)
            }
        }
    }
    func testDefineResoleveProperties() throws {
        let global = SymbolTable()
        let firstLocal = SymbolTable(outer: global)
        let secondLocal = SymbolTable(outer: firstLocal)
        let expected = [
            Symbol(name: "a", scope: .PROPETRY, index: 0),
            Symbol(name: "c", scope: .PROPETRY, index: 1),
            Symbol(name: "e", scope: .PROPETRY, index: 2),
            Symbol(name: "f", scope: .PROPETRY, index: 3),
        ]
        for (index, symbol) in expected.enumerated() {
            _ = global.define(name: symbol.name, index: index, scope: .PROPETRY)
        }
        for table in [global, firstLocal, secondLocal] {
            for symbol in expected {
                let result = try XCTUnwrap(table.resolve(symbol.name))
                XCTAssertEqual(result, symbol)
            }
        }
    }
    func testResolveFree() throws {
        let global = SymbolTable()
        _ = global.define("a")
        _ = global.define("b")
        let firstLocal = SymbolTable(outer: global)
        _ = firstLocal.define("c")
        _ = firstLocal.define("d")
        let secondLocal = SymbolTable(outer: firstLocal)
        _ = secondLocal.define("e")
        _ = secondLocal.define("f")
        let tests: [(table: SymbolTable, expectedSymbols: [Symbol], expectedFreeSymbols: [Symbol])] = [
            (firstLocal,
             [
                Symbol(name: "a", scope: .GLOBAL, index: 0),
                Symbol(name: "b", scope: .GLOBAL, index: 1),
                Symbol(name: "c", scope: .LOCAL, index: 0),
                Symbol(name: "d", scope: .LOCAL, index: 1),
             ],
             []
            ),
            (secondLocal,
             [
                Symbol(name: "a", scope: .GLOBAL, index: 0),
                Symbol(name: "b", scope: .GLOBAL, index: 1),
                Symbol(name: "c", scope: .FREE, index: 0),
                Symbol(name: "d", scope: .FREE, index: 1),
                Symbol(name: "e", scope: .LOCAL, index: 0),
                Symbol(name: "f", scope: .LOCAL, index: 1),
             ],
             [
                Symbol(name: "c", scope: .LOCAL, index: 0),
                Symbol(name: "d", scope: .LOCAL, index: 1),
             ]
            ),
        ]
        for t in tests {
            for s in t.expectedSymbols {
                guard let result = t.table.resolve(s.name) else {
                    XCTFail("シンボル「\(s.name)」の解決に失敗した。")
                    continue
                }
                XCTAssertEqual(s, result)   // 解決結果の確認
            }
            // 自由シンボルテーブルの確認
            XCTAssertEqual(t.expectedFreeSymbols.count, t.table.freeSymbols.count)
            for (i, s) in t.expectedFreeSymbols.enumerated() {
                let result = t.table.freeSymbols[i]
                XCTAssertEqual(s, result)
            }
        }
    }
    func testResolveUnresolvableFree() {
        let global = SymbolTable()
        _ = global.define("a")
        let firstLocal = SymbolTable(outer: global)
        _ = firstLocal.define("c")
        let secondLocal = SymbolTable(outer: firstLocal)
        _ = secondLocal.define("e")
        _ = secondLocal.define("f")
        let expected: [Symbol] = [
            Symbol(name: "a", scope: .GLOBAL, index: 0),
            Symbol(name: "c", scope: .FREE, index: 0),
            Symbol(name: "e", scope: .LOCAL, index: 0),
            Symbol(name: "f", scope: .LOCAL, index: 1),
        ]
        for s in expected {
            guard let result = secondLocal.resolve(s.name) else {
                XCTFail("シンボル「\(s.name)」の解決に失敗した。")
                continue
            }
            XCTAssertEqual(s, result)   // 解決結果の確認
        }
        // 解決失敗の確認
        let expectedUnresolvable = ["b", "d"]
        for name in expectedUnresolvable {
            XCTAssertNil(secondLocal.resolve(name)) // 解決失敗(nil)
        }
    }
    func testDefineAndResolveFunctionName() throws {
        let global = SymbolTable()
        _ = global.define(functionName: "a")
        let exptected = Symbol(name: "a", scope: .FUNCTION, index: 0)
        let result = try XCTUnwrap(global.resolve(exptected.name))
        XCTAssertEqual(exptected, result)
    }
    func testShadowingFunctionName() throws {
        let global = SymbolTable()
        _ = global.define(functionName: "a")
        _ = global.define("a")
        let exptected = Symbol(name: "a", scope: .GLOBAL, index: 0)
        let result = try XCTUnwrap(global.resolve(exptected.name))
        XCTAssertEqual(exptected, result)
    }
}

