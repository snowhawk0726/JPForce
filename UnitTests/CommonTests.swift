//
//  CommonTests.swift
//  (EvaluatorとVMの共通テストを行う)
//  UnitTests
//
//  Created by 佐藤貴之 on 2025/12/14.
//

import XCTest

final class CommonTests: XCTestCase {
    typealias VmTestCase = (input: String, expected: Any?)
    //
    override func setUpWithError() throws {
    }
    override func tearDownWithError() throws {
    }
    func testLhsAssingments() throws {
        let testPattern: [VmTestCase] = [
        ("xに1を代入。x。",1),
        ("yは1。yをxに代入。x。",1),
        ("配列【1,2,3】の1に0を代入。",[1,0,3]),
        ("aは配列【1,2,3】。iは1。aのiに0を代入。a。",[1,0,3]),
        ("aは配列【1,2,3】。iは1。aの1にaの2を足す。",5),
        ("1をxに代入し、xと１を足し、cに代入。c。",2),
        ("1と2を積む。aとbに得、aとbを足す。",3),
        ("xと１を足し、cに代入。c。","識別子『x』が定義されていない。"),
        ]
        print("評価器テスト開始！！")
        try evaluateTests(with: testPattern)
        print("翻訳器・VMテスト開始！！")
        try runVmTests(with: testPattern)
    }
    // MARK: - Helpers
    private func evaluateTests(with tests: [VmTestCase]) throws {
        for t in tests {
            print("テスト開始：「\(t.input)」")
            let evaluated = try XCTUnwrap(testEvaluator(t.input))
            try testExpectedObject(t.expected, evaluated)
            print("テスト結果：\(evaluated.string)")
        }
    }
    private func runVmTests(with tests: [VmTestCase]) throws {
        for t in tests {
            var result: JpfObject?
            print("テスト開始：「\(t.input)」")
            let lexer = Lexer(t.input)
            let parser = Parser(lexer)
            guard let program = parser.parseProgram() else {
                XCTAssertEqual(t.expected as? String, parser.errors.first!)
                print("テスト結果：\(parser.errors.first!)")
                continue
            }
            let compiler = Compiler(from: program)
            if let error = compiler.compile() {
                result = error          // コンパイルエラー
            } else {
                let vm = VM(with: compiler.bytecode)
                result = vm.run() ?? vm.stackTop    // 実行結果
            }
            try testExpectedObject(t.expected, result)
            print("テスト結果：\(result?.string ?? "nil")")
        }
    }
    private func testExpectedObject(_ expected: Any?, _ actual: JpfObject?) throws {
        switch expected {
        case let integer as Int:
            try testIntegerObject(Int64(integer), actual)
        case let boolean as Bool:
            try testBooleanObject(boolean, actual)
        case let string as String:
            try testStringObject(string, actual)
        case let array as [Any]:
            guard let actualArray = actual as? JpfArray else {
                throw XCTSkip("Expected array, but got \(String(describing: actual))")
            }
            XCTAssertEqual(array.count, actualArray.count.number, "Array count mismatch")
            try array.enumerated().forEach { try testExpectedObject($1, actualArray[$0]) }
        case let expectedDict as [JpfHashKey: Any]:
            let actualDict = try XCTUnwrap(actual as? JpfDictionary)
            XCTAssertEqual(expectedDict.count, actualDict.pairs.count)
            for (expectedKey, expectedValue) in expectedDict {
                let pair = try XCTUnwrap(actualDict[expectedKey])
                try testExpectedObject(expectedValue, pair.1)
            }
        case nil:
            XCTAssertTrue(actual?.isNull ?? true, "Expected nil or Null, but got \(String(describing: actual))")
        default:
            break
        }
    }
    private func testIntegerObject(_ expected: Int64, _ actual: JpfObject?) throws {
        let integer = try XCTUnwrap(actual as? JpfInteger, "実際は、\(String(describing: actual))")
        XCTAssertEqual(Int(expected), integer.value)
    }
    private func testBooleanObject(_ expected: Bool, _ actual: JpfObject?) throws {
        let boolean = try XCTUnwrap(actual as? JpfBoolean, "実際は、\(String(describing: actual))")
        XCTAssertEqual(expected, boolean.value)
    }
    private func testStringObject(_ expected: String, _ actual: JpfObject?) throws {
        switch actual {
        case let string as JpfString:
            XCTAssertEqual(expected, string.value)
        case let error as JpfError:
            XCTAssertEqual(expected, error.message)
        default:
            XCTFail("期待値は「\(expected)」だが、実際は「\(String(describing: actual))」。")
        }
    }
}
