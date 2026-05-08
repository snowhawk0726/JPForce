//
//  ConstAnalysisTest.swift
//  UnitTests
//
//  Created by 佐藤貴之 on 2026/04/26.
//

import XCTest

final class ConstAnalysisTests: XCTestCase {
    override func setUpWithError() throws {
    }
    override func tearDownWithError() throws {
    }
    func testDefineStatements() throws {
        let testPatterns: [(input: String, ident: String, constant: Int, isRedefined: Bool)] = [
            ("aは11。", "a", 11, false),
            ("『表示』は22。", "表示", 22, true),
        ]
        for t in testPatterns {
            print("テスト開始：\(t.input)")
            let program = parseProgram(with: t.input)!
            let compiler = Compiler(from: program)
            let analyzed = compiler.analyze()
            XCTAssertNil(analyzed)
            let symbol = try XCTUnwrap(compiler.symbolTable.resolve(t.ident))
            XCTAssertEqual(symbol.scope, .GLOBAL)
            XCTAssertEqual(compiler.environment[symbol.name]?.number, t.constant)
            XCTAssertEqual(symbol.isRedefined, t.isRedefined)
            print("テスト終了：識別子「\(t.ident)」、定数「\(t.constant)」、再定義「\(t.isRedefined)」")
        }
    }
    func testRangeLiterals() throws {
        let gteOne = (JpfInteger(value: 1), Token(.GTEQUAL))
        let underTen = (JpfInteger(value: 10), Token(.UNDER))
        let testRange = JpfRange(lowerBound: gteOne, upperBound: underTen)
        let testPatterns: [(input: String, expected: JpfObject?)] = [
            ("1以上10未満", testRange),
            ("a以上b未満", nil),
            ("""
                aは1。bは10。
                a以上b未満
            """, testRange),
            ("範囲【1と0を足す以上、11から1を引く未満】", testRange),
        ]
        for t in testPatterns {
            print("テスト開始：\(t.input)")
            let program = parseProgram(with: t.input)!
            let compiler = Compiler(from: program)
            guard let constant = compiler.analyze() else {
                XCTAssertNil(t.expected)
                print("テスト終了：nil")
                continue
            }
            XCTAssertTrue(constant.isEqual(to: t.expected!))
            print("テスト終了：\(constant.string)")
        }
    }
    // MARK: - Helpers
}
