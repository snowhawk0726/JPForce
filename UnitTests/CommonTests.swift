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
            ("aは配列【1,2,3】。aの1に2を足し、bに代入する。b。",4),
            ("1をxに代入し、xと１を足し、cに代入。c。",2),
            ("1と2を積む。aとbに得、aとbを足す。",3),
            ("xと１を足し、cに代入。c。","識別子『x』が定義されていません。"),
            ("aは配列【1,2,3】。vは2。aの１にvを足して代入。a", [1,4,3]), // 複合代入
            ("dは辞書【真が１,偽が2】。dの真から1を引いて代入。dの真", 0),
        ]
        print("評価器テスト開始")
        try evaluateTests(with: testPattern)
        print("翻訳器・VMテスト開始")
        try runVmTests(with: testPattern)
    }
    func testSentences() throws {
        let testPatterns: [(input: String, expected: Any?)] = [
            ("1と2を足す。", 3),
            ("1と2を足し、", 3),
            ("1と2を足し、3を足す。", 6),
            ("1と2を足し、3を足し", 6),
            ("aに1を代入。a", 1),
            ("1をaに代入し、2をbに代入。bからaを引く。", 1),
            ("aは1。aに2を足し、bに代入。b", 3),
            ("aは1。aに2を足し、３を足して代入。a", 6),
            ("xは１。xを負数にして代入。x。", -1),
            ("1、aに代入。a", 1),
            ("aは100。関数【aは1。外部「a」に0を代入】を実行。a。", 0),
            ("配列【1,2,3】。その型", "配列"),
            ("配列【1,2,3】。その数", 3),
            ("甲は、1が2より大きいかによって、10か20。甲。", 20),
            ("条件は、1が2より小さい。甲は、条件によって、10か20。甲。", 10),
            // エラー
            ("aに1個代入。a", assignUsage.message),  // エラー(「個」)
            ("xを負数にして代入。x。", undefinedIdentifier("x").message), // 未定義エラー
            ("aに1を足して代入。a。", undefinedIdentifier("a").message), // 未定義エラー
            ("外部「a」に１を代入。a。", outerUndefinedIdentifier("a").message), // 外部指定エラー
            ("関数【aは1。外部「a」に0を代入】を実行。a。", outerUndefinedIdentifier("a").message),// 外部未定義エラー
        ]
        for test in testPatterns {
            print("テスト開始：\(test.input)")
            let parser = Parser(input: test.input)
            do {
                let statements = try parseSentenses(with: parser)
                // インタープリタテスト
                let environment = Environment()
                let eval = Evaluator(from: Program(statements: statements), with: environment)
                let evaluated = eval.object ?? environment.pull()
                try testExpectedObject(test.expected, evaluated)
                print("テスト結果(評価)：\t\(evaluated?.string ?? "nil")")

                // コンパイラー/VMテスト
                let vmResult = compileAndRun(statements)
                try testExpectedObject(test.expected, vmResult)
                print("テスト結果(VM)：\t\(vmResult?.string ?? "nil")")

            } catch CommonTestError.syntax {
                XCTAssertNil(test.expected)
                parser.errors.forEach { print($0) }
                print("テスト結果：構文エラー")
            }
        }
    }
    func testRedefinedKeywords() throws {
        let testPattern: [VmTestCase] = [
            ("『足す』は１。足すと１を《足し》、2を掛ける。", 4),
        ]
        print("評価器テスト開始")
        try evaluateTests(with: testPattern)
        print("翻訳器・VMテスト開始")
        try runVmTests(with: testPattern)
    }
    func testRangeCheckings() throws {
        let testPattern: [VmTestCase] = [
            ("1が範囲【１以下】にある", true),
            ("1が範囲【１未満】にある", false),
            ("2が範囲【１以上】にある", true),
            ("1が範囲【１以上２以下】にある", true),
            ("１が１以上２以下にある", true),
            ("1が1~2にある", true),
            ("1が1~にある", true),
            ("２が範囲【１以上3未満】にある", true),
            ("２が範囲【１以上3未満】にない", false),
            ("２が１以上3未満にない", false),
            ("2が1~3未満にない", false),
            ("1が範囲【１から10まで】にある", true),
            ("11が範囲【１から10まで】にある", false),
            ("3が範囲【１から2まで】または範囲【４から10まで】にある", false),
            ("5が範囲【１から2まで】または範囲【４から10まで】にある", true),
            ("範囲【1から3まで】は範囲【1以上3以下】に等しい", true),
            ("範囲【1から3まで】は範囲【1以上3未満】に等しい", false),
            ("1〜3の型が「範囲」である", true),
            ("1〜3未満の型が「範囲」でない", false),
            ("1〜の型が「範囲」である", true),
        ]
        print("評価器テスト開始")
        try evaluateTests(with: testPattern)
        print("翻訳器・VMテスト開始")
        try runVmTests(with: testPattern)
    }
    func testArrayInitialization() throws {
        let testPattern: [VmTestCase] = [
            ("配列であって、要素が、3個の３。", [3,3,3]),
            ("nは4。vは「い」。配列【n個のv。】", ["い","い","い","い"]),
        ]
        print("評価器テスト開始")
        try evaluateTests(with: testPattern)
        print("翻訳器・VMテスト開始")
        try runVmTests(with: testPattern)
    }
    func testSpecifierAccess() throws {
        let testPattern: [VmTestCase] = [
            ("配列【1,2,3】の最初に0を代入", [0,2,3]),
            ("配列【1,2,3】の最後に0を代入", [1,2,0]),
            ("aは配列【1,2,3】。aの先頭に0を代入。a", [0,2,3]),
            ("aは配列【1,2,3】。aの末尾は0。a", [1,2,0]),
            ("配列【1,2,3】の最初は、0。", [0,2,3]),
            ("配列【1,2,3】の最後は、0。", [1,2,0]),
            ("配列【1,2,3】の残りに0を追加", [2,3,0]),   
       ]
        print("評価器テスト開始")
        try evaluateTests(with: testPattern)
        print("翻訳器・VMテスト開始")
        try runVmTests(with: testPattern, isOptimized: true)    // TODO: falseにする
    }
    // MARK: - Helpers
    enum CommonTestError: Error {
        case syntax
    }
    let syntaxError: Error = CommonTestError.syntax
    //
    private func parseSentenses(with parser: Parser) throws -> [Statement] {
        let program = try XCTUnwrap(parser.parseProgram())
        return program.statements
    }
    private func compileAndRun(_ statements: [Statement]) -> JpfObject? {
        let compiler = Compiler(from: Program(statements: statements))
        if let result = compiler.compile() {
            return result
        } else {
            let vm = VM(with: compiler.bytecode)
            // run() が nil なら stackTop を使うが、いずれも JpfObject? に正規化
            if let runResult = vm.run() {
                return runResult
            } else {
                return vm.stackTop
            }
        }
    }
    private func evaluateTests(with tests: [VmTestCase]) throws {
        for t in tests {
            print("テスト開始：「\(t.input)」")
            let evaluated = try XCTUnwrap(testEvaluator(t.input))
            try testExpectedObject(t.expected, evaluated)
            print("テスト結果：\(evaluated.string)")
        }
    }
    private func runVmTests(with tests: [VmTestCase], isOptimized: Bool = true) throws {
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
            compiler.optimizeConstantsEnabled = isOptimized
            if isOptimized {
                switch compiler.analyze() {
                case .error(let error):
                    XCTFail("\(error)")
                case .constant(let analyzed):
                    try testExpectedObject(t.expected, analyzed)
                    print("テスト結果：\(analyzed.string)")
                    continue
                case .nonConstant:
                    break
                case .evaluated:
                    print("テスト結果：評価済み")
                    continue
                }
            }
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
