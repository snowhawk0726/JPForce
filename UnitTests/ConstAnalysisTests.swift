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
    func testConstants() throws {
        let elements: [JpfObject] = [JpfInteger(value: 1),JpfInteger(value: 2),JpfInteger(value: 3)]
        let pairs: [JpfHashKey : (key: JpfObject, value: JpfObject)] = {
            let key1 = JpfInteger(value: 1)
            let key2 = JpfInteger(value: 2)
            return [
                key1.hashKey: (key: key1, value: JpfBoolean.TRUE),
                key2.hashKey: (key: key2, value: JpfBoolean.FALSE),
            ]
        }()
        let testPatterns: [(input: String, expected: JpfObject?)] = [
            ("1", JpfInteger(value: 1)),
            ("真", JpfBoolean.TRUE),
            ("「文字列」", JpfString(value: "文字列")),
            ("配列【1,2,3】", JpfArray(elements: elements)),
            ("辞書【1が真、2が偽】", JpfDictionary(pairs: pairs)),
            ("1を", JpfPhrase(value: JpfInteger(value: 1), particle: Token(.WO))),
            ("配列【a,b,c】", nil),
            ("辞書【1がa、2がb】", nil),
            ("aと", nil),
        ]
        for t in testPatterns {
            print("テスト開始：\(t.input)")
            let program = parseProgram(with: t.input)!
            let compiler = Compiler(from: program)
            switch compiler.analyze() {
            case .constant(let const):
                XCTAssertTrue(const.isEqual(to: t.expected!))
                print("テスト終了：.constant(\(const.string))")
            case .nonConstant:
                XCTAssertNil(t.expected)
                print("テスト終了：.nonConstant")
            default:
                XCTFail()
            }
        }
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
            XCTAssertEqual(analyzed, .evaluated)
            let symbol = try XCTUnwrap(compiler.symbolTable.resolve(t.ident))
            XCTAssertEqual(symbol.scope, .GLOBAL)
            XCTAssertEqual(compiler.environment[symbol.name]?.number, t.constant)
            XCTAssertEqual(symbol.isRedefined, t.isRedefined)
            print("テスト終了：識別子「\(t.ident)」、定数「\(t.constant)」、再定義「\(t.isRedefined)」")
        }
    }
    func testRangeLiterals() throws {
        let gteOne = RangeBoundary(value: JpfInteger(value: 1), inclusive: true)
        let underTen = RangeBoundary(value: JpfInteger(value: 10), inclusive: false)
        let testRange = JpfRange(lowerBound: gteOne, upperBound: underTen)
        let testPatterns: [(input: String, expected: Any?)] = [
            ("1以上10未満", testRange),
            ("a以上b未満", "識別子『a』が定義されていません。"),
            ("""
                aは1。bは10。
                a以上b未満
            """, testRange),
            ("範囲【1と0を足す以上、10に1を掛ける未満】", testRange),
            ("1〜10未満", testRange),
        ]
        for t in testPatterns {
            print("テスト開始：\(t.input)")
            let program = parseProgram(with: t.input)!
            let compiler = Compiler(from: program)
            switch compiler.analyze() {
            case .constant(let constant):
                XCTAssertTrue(constant.isEqual(to: t.expected as! JpfObject))
                print("テスト終了：\(constant.string)")
            case .error(let message):
                XCTAssertEqual(message, t.expected as! String)
                print("テスト終了：エラー「\(message)」")
            case .nonConstant:
                guard let error = compiler.compile() as? JpfError else {
                    XCTFail("should not be nonConstant")
                    return
                }
                XCTAssertEqual(error.message, t.expected as! String)
                print("テスト終了：エラー「\(error.message)」")
            case .evaluated:
                XCTFail("should not be evaluated")
            }
        }
    }
    func testEvaluatedConstant() throws {
        let testPatterns: [(input: String, expected: JpfObject?, numOfStacks: Int)] = [
            /* 評価定数値はスタックからpullしているため、スタック数は、インタプリタより1少なくなる。 */
            ("2から2を引き、1を足す", JpfInteger(value: 1), 0),
            ("1を積む", nil, 1),
            ("1を積み、1と1を足す", JpfInteger(value: 2), 1),
            ("空にする", nil, 0),
            ("表示する", JpfError("「表示」には１つ以上の入力が必要です。仕様：(〜と…)〜を表示する。"), 0),
            ("aを表示", JpfError("識別子『a』が定義されていません。"), 0),
            ("配列【1,2,3】。その最後。表示。", nil, 0),
        ]
        for t in testPatterns {
            print("テスト開始：\(t.input)")
            let program = parseProgram(with: t.input)!
            let compiler = Compiler(from: program)
            switch compiler.analyze() {
            case .constant(let constant):
                XCTAssertTrue(constant.isEqual(to: t.expected!))
                XCTAssertEqual(compiler.count, t.numOfStacks)
                print("テスト終了：.constant(\(constant.string))")
            case .evaluated:
                XCTAssertNil(t.expected)
                XCTAssertEqual(compiler.count, t.numOfStacks)
                print("テスト終了：.evaluated")
            case .error(let message):
                let err = try XCTUnwrap(t.expected as? JpfError)
                XCTAssertEqual(message, err.message)
                XCTAssertEqual(compiler.count, t.numOfStacks)
                print("テスト終了：.error(\(message))")
            case .nonConstant:
                let result = try XCTUnwrap(compiler.compile() as? JpfError) // 翻訳エラー
                let err = try XCTUnwrap(t.expected as? JpfError)
                XCTAssertEqual(result.message, err.message)
                XCTAssertEqual(compiler.count, t.numOfStacks)
                print("テスト終了：.nonConstant(\(result.message))")
            }
        }
    }
    func testExpressions() throws {
        let testPatterns: [(input: String, expected: JpfObject?)] = [
            ("１または2", JpfArray(elements: [JpfInteger(value: 1), JpfInteger(value: 2)])),
            ("配列【1,2,3】の1", JpfInteger(value: 2)),
            ("配列【1,2,3】。その先頭", JpfInteger(value: 1)),
            ("条件は真。条件によって1か2", JpfInteger(value: 1)),
            ("甲は、1が2より大きいかによって、10か20。甲。", JpfInteger(value: 20)),
            ("条件は真。条件によってaかb", JpfError("識別子『a』が定義されていません。")),
            ("甲は、aがbより大きいかによって、10か20。甲。", JpfError("識別子『a』が定義されていません。")),
        ]
        for t in testPatterns {
            print("テスト開始：\(t.input)")
            let program = parseProgram(with: t.input)!
            let compiler = Compiler(from: program)
            switch compiler.analyze() {
            case .constant(let constant):
                XCTAssertTrue(constant.isEqual(to: t.expected!))
                print("テスト終了：.constant(\(constant.string))")
            case .evaluated:
                XCTAssertNil(t.expected)
                print("テスト終了：.evaluated")
            case .error(let message):
                let err = try XCTUnwrap(t.expected as? JpfError)
                XCTAssertEqual(message, err.message)
                print("テスト終了：.error(\(message))")
            case .nonConstant:
                let result = try XCTUnwrap(compiler.compile() as? JpfError) // 翻訳エラー
                let err = try XCTUnwrap(t.expected as? JpfError)
                XCTAssertEqual(result.message, err.message)
                print("テスト終了：.nonConstant(\(result.message))")
            }
        }
    }
}
