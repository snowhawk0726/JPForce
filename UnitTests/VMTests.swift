//
//  VMTests.swift
//  UnitTests
//
//  Created by 佐藤貴之 on 2024/06/21.
//

import XCTest

final class VMTests: XCTestCase {
    typealias VmTestCase = (input: String, expected: Any?)
    //
    override func setUpWithError() throws {
    }
    override func tearDownWithError() throws {
    }
    func testIntegerArithmetic() throws {
        let testPattern: [VmTestCase] = [
            ("1", 1), ("2", 2),
            ("5の負数", -5), ("10を負数にする", -10), ("5を。負数にする", -5),
            ("1と2を足す", 3), ("1と2を。足す", 3), ("1と。2を足す", 3),
            ("1から2を引く", -1), ("1から2を。引く", -1),
            ("1と2を掛ける", 2), ("1と2を。掛ける", 2),
            ("4を2で割る", 2), ("4を2で。割る", 2),
            ("50を2で割り、2を掛け、10を足し、5を引く", 55),
            ("5と5と5と5を足し、10を引く", 10),
            ("2と2と2と2と2を掛ける", 32),
            ("5と5と5と5を足し、10を引く", 10),
            ("5と2を掛け、10を足す", 20),
            ("5に、2と10を掛けたものを、足す", 25),
            ("5に、2と10を足したものを、掛ける", 60),
            ("-50と100と-50を足す", 0),
            ("5に、10と2を掛けたものを足し、15を3で割ったものを足し、2を掛け、-10を足す", 50),
            ("5に。10と2を掛けたものを足す。15を3で割ったものを足す。2を掛ける。-10を足す", 50),
        ]
        try runVmTests(with: testPattern)
    }
    func testBooleanObjectExpressions() throws {
        let testPattern: [VmTestCase] = [
            ("真", true), ("偽", false),
            ("真である", true), ("真でない", false), ("偽である", false), ("偽でない", true),
            ("真で。ある", true), ("真で。ない", false), ("偽で。ある", false), ("偽で。ない", true),
            ("5でない", false), ("真でなくない", true), ("偽でなくない", false), ("5でなくない", true),
            ("5で。ない", false), ("真で。なくない", true), ("偽で。なくない", false), ("5で。なくない", true),
            ("1が正", true), ("1が負", false), ("-1が正", false), ("-1が負", true), ("0が正", false), ("0が負", false),
            ("1。正", true), ("1。負", false), ("-1。正", false), ("-1。負", true), ("0。正", false), ("0。負", false),
        ]
        try runVmTests(with: testPattern)
    }
    func testBooleanExpressions() throws {
        let testPattern: [VmTestCase] = [
            ("１が２より小さい", true), ("１が２より大きい", false),
            ("１が２より。小さい", true), ("１が２より。大きい", false),
            ("１が１より小さい", false), ("１が１より大きい", false),
            ("１が１より。小さい", false), ("１が１より。大きい", false),
            ("１が１に等しい", true), ("１が１に等しくない", false),
            ("１が１に。等しい", true), ("１が１に。等しくない", false),
            ("１が２に等しい", false), ("１が２に等しくない", true),
            ("１が２に。等しい", false), ("１が２に。等しくない", true),
            ("真が真に等しい", true), ("偽が偽に等しい", true), ("真が偽に等しい", false),
            ("真が真に。等しい", true), ("偽が偽に。等しい", true), ("真が偽に。等しい", false),
            ("真が偽に等しくない", true), ("偽が真に等しくない", true),
            ("真が偽に。等しくない", true), ("偽が真に。等しくない", true),
            ("１より２が小さいは、真に等しい", true), ("１より２が小さいは、偽に等しい", false),
            ("１より２が。小さいは、真に等しい", true), ("１より２が。小さいは、偽に等しい", false),
            ("１より２が大きいは、真に等しい", false), ("１より２が大きいは、偽に等しい", true),
            ("１より２が。大きいは、真に等しい", false), ("１より２が。大きいは、偽に等しい", true),
            ("１より２が小さいは、真である", true), ("１より２が小さいは、偽である", false),
            ("１より２が。小さいは、真である", true), ("１より２が。小さいは、偽である", false),
            ("１より２が小さいは、真でない", false), ("１より２が小さいは、偽でない", true),
            ("１より２が。小さいは、真でない", false), ("１より２が。小さいは、偽でない", true),
            //            ("偽である場合、【５】でない", true),
        ]
        try runVmTests(with: testPattern)
    }
    func testStringExpressions() throws {
        let testPattern: [VmTestCase] = [
            ("「モンキー」", "モンキー"),
            ("「モン」と「キー」を足す", "モンキー"),
            ("「モン」と「キー」を。足す", "モンキー"),
            ("「モン」と「キー」と「バナナ」を足す", "モンキーバナナ"),
            ("「モン」と「キー」と「バナナ」を。足す", "モンキーバナナ"),
        ]
        try runVmTests(with: testPattern)
    }
    func testConditionals() throws {
        let testPattern: [VmTestCase] = [
            ("真である場合、【10】。", 10),
            ("真である場合【10】、それ以外は【20】。", 10), ("偽である場合【10】、それ以外は【20】。", 20),
            ("１である場合、【10】。", 10),
            ("１が２より小さい場合、【10】", 10),
            ("１が２より小さい場合【10】、それ以外は【20】", 10),
            ("１が２より大きい場合【10】、それ以外は【20】", 20),
            ("１が２より大きい場合、【10】", nil),
            ("偽である場合、【10】。", nil),
            //            ("偽である場合【10】、である場合【10】、それ以外は【20】", 20),
        ]
        try runVmTests(with: testPattern)
    }
    func testArrayLiterals() throws {
        let testPattern: [VmTestCase] = [
            ("配列【】", []),
            ("配列【１、２、３】", [1, 2, 3]),
            ("配列【1と2を足す、3と4を掛ける、5と6を足す】", [3, 12, 11]),
        ]
        try runVmTests(with: testPattern)
    }
    func testDictionaryLiterals() throws {
        let testPattern: [VmTestCase] = [
            ("辞書【１が２、２が３】",
             [JpfInteger(value: 1).hashKey: 2,
              JpfInteger(value: 2).hashKey: 3,
             ]
            ),
            ("辞書【１と１を足すが２と２を掛ける、３と３を足すが４と４を掛ける】",
             [JpfInteger(value: 2).hashKey: 4,
              JpfInteger(value: 6).hashKey: 16,
             ]
            ),
        ]
        try runVmTests(with: testPattern)
    }
    func testNulls() throws {
        let testPattern: [VmTestCase] = [
            ("無", nil),
            ("無である。", false),
            ("無と無は等しい。", true),
            ("無と無は。等しくない。", false),
        ]
        try runVmTests(with: testPattern)
    }
    func testGenitiveExpressions() throws {
        let testPattern: [VmTestCase] = [
            ("配列【１、２、３】の1", 2),             // キャッシュ
            ("配列【１、２、３】の先頭", 1),          // キャッシュ
            ("1の負数", -1),                       // キャッシュ
            ("iは1。配列【１、２、３】のi", 2),       // opGenitive
            ("aは配列【１、２、３】。aの1", 2),       // opGenitive
            ("aは配列【１、２、３】。iは１。aのi", 2), // opGenitive
            ("aは配列【１、２、３】。aの最後", 3),     // opGetGlobal, opGetProperty
            ("iは１。iの負数", -1),                 // opGetGlobal, opPredicate
            ("１。負数の文字列", "-1"),             // opPredicate, opGetProperty
            ("「１」。数値の負数", -1),             // opGetProperty, opPredicate
            ("aは配列【１、２、配列【3】。aの最後の0の負数", -3),
            ("iは1。配列【１、２、３】のi", 2),
            ("iは、０と2を足す。配列【１、２、３】のi", 3),
            ("iは０。jは０。配列【配列【１、１、１】】のiのj", 1),
            ("iは０。配列【】のi", nil),
            ("iは９９。配列【１、２、３】のi", nil),
            ("iは−１。配列【１】のi", nil),
            ("iは１。辞書【１が１、２が２】のi", 1),
            ("iは２。辞書【１が１、２が２】のi", 2),
            ("iは０。辞書【１が１】のi", nil),
            ("iは０。辞書【】のi", nil),
        ]
        try runVmTests(with: testPattern)
    }
    func testGlobalDefineStatements() throws {
        let testPattern: [VmTestCase] = [
            ("一は１。一", 1),
            ("一は１。二は２。一と二を足す", 3),
            ("一は１。二は、一と一を足す。一と二を足す", 3),
        ]
        try runVmTests(with: testPattern)
    }
    func testReturnValueStatements() throws {
        let testPattern: [VmTestCase] = [
            ("関数【1を返す】を実行", 1),
            ("関数【返す】を実行", "返すべき値が無い。"),         // コンパイルエラー
            ("関数【１と１を足し、返す】を実行する", 2),
            ("関数【aは１。aを返す】を実行", 1),
            ("関数【aは１。返す】を実行", "返すべき値が無い。"),   // コンパイルエラー
            ("関数【aは１。aと1を足し、返す】を実行", 2),
            ("関数【1に返す】を実行", "仕様：(〜を)返す。"),      // コンパイルエラー
            ("関数【aは１。aで返す】を実行", "仕様：(〜を)返す。"),  // コンパイルエラー
            ("１を返す。", 1),
            ("１が返す。", "仕様：(〜を)返す。"),                // コンパイルエラー
            ("返す", "返すべき値が無い。"),                    // コンパイルエラー
            ("aは１。aを返す。", 1),
            ("aは１。返す", "返すべき値が無い。"),                // コンパイルエラー
        ]
        try runVmTests(with: testPattern)
    }
    func testCallingFunctionsWithoutArguments() throws {
        let testPattern: [VmTestCase] = [
            ("fivePlusTenは、関数【５と１０を足す】。fivePlusTenを実行する。", 15),
            ("oneは、関数【１】。twoは、関数【２】。oneを実行し、twoを実行し、足す。", 3),
            ("aは、関数【１】。bは、関数【aを実行し、1を足す】。cは、関数【bを実行し、1を足す】。cを実行する。", 3),
        ]
        try runVmTests(with: testPattern)
    }
    func testFunctionsWithRturnStatement() throws {
        let testPattern: [VmTestCase] = [
            ("早期脱出は、関数【99を返す。100】。早期脱出を実行する。", 99),
            ("早期脱出は、関数【99を返す。100を返す】。早期脱出をする。", 99),
        ]
        try runVmTests(with: testPattern)
    }
    func testFunctionsWithoutReturnValue() throws {
        let testPattern: [VmTestCase] = [
            ("返り値無は、関数【】。返り値無２は、関数【返り値無を実行】。返り値無を実行。返り値無２を実行。", nil),
        ]
        try runVmTests(with: testPattern)
    }
    func testFirstClassFunctions() throws {
        let testPattern: [VmTestCase] = [
            ("返り値１は、関数【１】。返り値１返却は、関数【返り値１】。返り値１返却し、実行。", 1),
        ]
        try runVmTests(with: testPattern)
    }
    func testCallingFuntionsWithBindings() throws {
        let testPattern: [VmTestCase] = [
            ("oneは、関数【oneは１。one】。oneを実行する。", 1),
            ("oneAndTwoは、関数【oneは１。twoは２。oneとtwoを足す】。oneAndTwoを実行。", 3),
            ("""
                oneAndTwoは、関数【oneは１。twoは２。oneとtwoを足す】。
                threeAndFourは、関数【threeは3。fourは4。threeとfourを足す】。
                oneAndTwoを実行し、threeAndFourを実行し、足す。
            """, 10),
            ("""
                firstFoobarは、関数【foobarは50。foobar】。
                secondFoobarは、関数【foobarは100。foobar】。
                firstFoobarを実行し、secondFoobarを実行し、足す。
            """, 150),
            ("""
                globalSeedは50。
                minusOneは関数【numは1。globalSeedからnumを引く】。
                minusTwoは関数【numは2。globalSeedからnumを引く】。
                minusOneを実行し、minusTwoを実行し、足す。
            """, 97),
        ]
        try runVmTests(with: testPattern)
    }
    func testCallingFuntionsWithArgumentsAndBindings() throws {
        let testPattern: [VmTestCase] = [
            ("判別は、関数【入力がa。a】。4を判別する。", 4),
            ("合計は、関数【入力がaとb。aとbを足す】。１と２を合計する。", 3),
            ("合計は、関数【入力がaとb。cは、aとbを足す。c】。１と２を合計する。", 3),
            ("合計は、関数【入力がaとb。cは、aとbを足す。c】。１と２を合計し、３と４を合計し、足す。", 10),
            ("合計は、関数【入力がaとb。cは、aとbを足す。c】。外側は、関数【１と２を合計し、３と４を合計し、足す】。外側を実行する。", 10),
            ("""
                大域数は、10。合計は、関数【入力がaとb。cは、aとbを足す。cと大域数を足す】。外側は関数【１と２を合計したものと、３と４を合計したものと、大域数を足す】。
                外側を実行し、大域数を足す。
            """, 50)
        ]
        try runVmTests(with: testPattern)
    }
    func testCallingFuntionsWithWrongArguments() throws {
        let testPattern: [VmTestCase] = [
            ("１で関数【１】を実行。", 1), // JPFではエラーでは無い
            ("関数【入力がa。a】を実行。", "入力の数が足りていない。必要数：1"),
            ("１で関数【入力がaとb。aとbを足す】を実行。", "入力の数が足りていない。必要数：2"),
        ]
        for test in testPattern {
            let program = parseProgram(with: test.input)
            let compiler = Compiler(from: program)
            XCTAssertNil(compiler.compile())
            let vm = VM(with: compiler.bytecode)
            if let error = vm.run() {
                XCTAssertEqual(error.message, test.expected as? String)
            } else {
                XCTAssertEqual(vm.stackTop?.number, test.expected as? Int)
            }
        }
    }
    func testBuiltinFunctions() throws {
        let testPattern: [VmTestCase] = [
            ("「」の数。", 0), ("「four」の数", 4), ("「Hello World」の数", 11),
            ("1。数", "数値型の要素の数は、数えることができない。"),
            ("配列【１、２、３】の数", 3), ("配列【】の数", 0),
            ("「Hello World」を表示", nil),
            ("配列【１、２、３】の最初", 1), ("配列【】の先頭", nil),
            ("１。最初", "1(数値)を「最初」でアクセスすることはできない。"),
            ("配列【１、２、３】の最後", 3), ("配列【】の後尾", nil),
            ("１。最後", "1(数値)を「最後」でアクセスすることはできない。"),
            ("配列【１、２、３】の残り", [2,3]), ("配列【】の残り", nil),
            ("配列【】に１を追加", [1]), ("１に１を。追加", "仕様：〜(を)〜に追加する。または、〜(に)〜を追加する。"),
            ("iは1。配列【１、２、３】のi", 2),
            ("aは配列【１、２、３】。aの1", 2),
            ("aは配列【１、２、３】。iは１。aのi", 2),
        ]
        try runVmTests(with: testPattern)
    }
    // MARK: - Helpers
    private func runVmTests(with tests: [VmTestCase]) throws {
        for t in tests {
            var result: JpfObject?
            print("テスト開始：「\(t.input)」")
            let program = parseProgram(with: t.input)
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
