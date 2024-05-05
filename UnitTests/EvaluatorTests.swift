//
//  EvaluatorTests.swift
//  UnitTests
//
//  Created by 佐藤貴之 on 2023/03/06.
//

import XCTest

final class EvaluatorTests: XCTestCase {
    override func setUpWithError() throws {
    }
    override func tearDownWithError() throws {
    }
    func testIntegerExpressions() throws {
        let testPatterns: [(input: String, exptected: Int)] = [
            ("5", 5), ("10", 10),
            ("-5", -5), ("-10", -10),
            ("5の負数", -5), ("10を負数にする", -10),
            ("-5の負数", 5), ("-10を負数にする", 10),
            ("5と5と5と5を足し、10を引く", 10),
            ("2と2と2と2と2を掛ける", 32),
            ("2を3から引く", 1),
            ("2と3を足し、10から引く", 5),
            ("-50と100と-50を足す", 0),
            ("5と2を掛け、10を足す", 20),
            ("5に、2に10を掛け、足す", 25),
            ("20に、2に-10を掛けたものを足す", 0),
            ("50を2で割り、2を掛け、10を足す", 60),
            ("2に、5と10を足したものを掛ける", 30),
            ("3と3と3を掛け、10を足す", 37),
            ("3に、3と3を掛けたものを掛け、10を足し", 37),
            ("5に、10と2を掛けたものを足し、15を3で割ったものを足し、2を掛け、-10を足す", 50),
            ("5で20を割る", 4),
            ("2と3を足したもので、15を割る", 3),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            guard let evaluated = testEvaluator(test.input) else {
                XCTFail("入力「\(test.input)」の評価に失敗した。")
                continue
            }
            try testObject(evaluated, with: test.exptected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testBooleanExpressions() throws {
        let testPatterns: [(input: String, exptected: Bool)] = [
            ("真", true), ("偽", false),
            ("1は2より小さい", true), ("1は2より大きい", false), ("1は1より小さい", false), ("1は1より大きい", false),
            ("1は１に等しい", true), ("１は1に等しくない", false), ("1は2に等しい", false), ("1は2に等しくない", true),
            ("2は1以上である", true), ("１は３以下である", true), ("1は3未満である", true),
            ("１は１以上である", true), ("1は1以下である", true), ("1は1未満である", false),
            ("真は真に等しい", true), ("偽は偽に等しい", true), ("真は偽に等しい", false), ("偽は真に等しくない", true),
            ("1は2より小さいは、真に等しい", true), ("1は2より小さいは、偽に等しい", false),
            ("1は2より大きいは、真に等しい", false), ("1は2より大きいは、偽に等しい", true),
            ("1は2である", false), ("1は1である", true), ("1は1でない", false),
            ("真は真である", true), ("偽は真でない", true), ("真は偽でない", true), ("1は真である", true),
            ("真である", true), ("真でない", false), ("偽である", false), ("偽でない", true),
            ("無である", false), ("無でない", true),
            // スタックに「１と」があるが、それを無視する
            ("１と真である", true), ("１と真でない", false), ("１と偽である", false), ("１と偽でない", true),
            ("１と無である", false), ("１と無でない", true),
            ("1が正", true), ("1が負", false), ("-1が正", false), ("-1が負", true), ("0が正", false), ("0が負", false),
            ("配列【１、２、３、４、５】は、配列【1,2,3,4,5】に等しくない。", false),
            ("辞書【「あ」が１、「い」が２、「う」が3】が辞書【「う」が3、「あ」が１、「い」が２】に等しい。", true),
            ("２が1以上3以下である", true), ("文字列は「」。文字列が空である", true)
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            guard let evaluated = testEvaluator(test.input) else {
                XCTFail("入力「\(test.input)」の評価に失敗した。")
                continue
            }
            try testObject(evaluated, with: test.exptected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testStringLiteral() throws {
        let input = "「今日のテスト！」"
        print("テストパターン: \(input)")
        guard let evaluated = testEvaluator(input) else {XCTFail();return}
        try testObject(evaluated, with: "今日のテスト！")
        print("テスト(\(evaluated))終了")
    }
    func testPredicateNots() throws {   // 真→偽, 偽→真, 無→真, else 偽
        let testPatterns: [(input: String, exptected: Bool)] = [
            ("真でない", false), ("偽でない", true), ("5でない", false),
            ("真でなくない", true), ("偽でなくない", false), ("5でなくない", true),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            guard let evaluated = testEvaluator(test.input) else {
                XCTFail("入力「\(test.input)」の評価に失敗した。")
                continue
            }
            try testObject(evaluated, with: test.exptected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testCaseExpressions() throws {
        let testPatterns: [(input: String, exptected: Any?)] = [
            ("真である場合、10", 10), ("偽である場合、10", nil), ("1である場合、10", 10),
            ("1が2より小さい場合、10", 10), ("1が2より大きい場合、10", nil),
            ("1が2より大きい場合、【10】、それ以外は、【20】。", 20),
            ("1が2より小さい場合、【10】、それ以外は、【20】。", 10),
            ("1が、1の場合、【1と2を足し】、3の場合、【1と4を足し】、それ以外は、1と5を足す。", 3),
            ("3が、1の場合、【1と2を足し】、3の場合、【1と4を足し】、それ以外は、1と5を足す。", 5),
            ("2が、1の場合、【1と2を足し】、3の場合、【1と4を足し】、それ以外は、1と5を足す。", 6),
            ("2が、範囲【1以下】の場合、【1と2を足し】、範囲【2以上4未満】の場合、【1と4を足し】、それ以外は、1と5を足す。", 5),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            if let evaluated = testEvaluator(test.input) {
                try testObject(evaluated, with: test.exptected)
                print("テスト(\(evaluated))終了")
            } else {
                XCTAssertNil(test.exptected, "構文解析、または評価失敗")
                print("テスト(nil)終了")
            }
        }
    }
    func testConditionalOperations() throws {
        let testPatterns: [(input: String, exptected: Any?)] = [
            ("甲は、1が2より大きいかによって、10か20。甲。", 20),
            ("条件は、1が2より小さい。甲は、条件によって、10か20。甲。", 10),
            ("年齢は17歳。入場者は、年齢が18以上であるかによって、「成人」か「未成年」。入場者。", "未成年"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            if let evaluated = testEvaluator(test.input) {
                try testObject(evaluated, with: test.exptected)
                print("テスト(\(evaluated))終了")
            } else {
                XCTAssertNil(test.exptected, "構文解析、または評価失敗")
                print("テスト(nil)終了")
            }
        }
    }
    func testReturnExpressions() throws {
        let testPatterns: [(input: String, exptected: Int)] = [
            ("10を返す。", 10), ("10を返し、9個", 10), ("２と5を掛けたものを返し、9人", 10), ("3と3を掛ける。２と5を掛けたものを返し、10から1を引く。", 10),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.exptected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testErrorHandling() throws {
        let testPatterns: [(input: String, exptected: String)] = [
            ("5と真を足す。", "エラー：「数値」と「真偽値」を足すことはできない。"),
            ("5と真を足す。5。", "エラー：「数値」と「真偽値」を足すことはできない。"),
            ("真を負数にする。", "エラー：「負数」には１つの数値入力が必要。仕様：〜の負数。または、〜を負数にする。"),
            ("真と偽を足す。", "エラー：「真偽値」と「真偽値」を足すことはできない。"),
            ("５。真と偽を足す。５。", "エラー：「真偽値」と「真偽値」を足すことはできない。"),
            ("10が1より大きい場合、真と偽を足す。", "エラー：「真偽値」と「真偽値」を足すことはできない。"),
            ("""
            10が1より大きい場合、【
                10が1より大きい場合、【
                    真と偽を足したものを返す。
                】。
                1を返す。
            】。
            """, "エラー：「真偽値」と「真偽値」を足すことはできない。"),
            ("foobar。", "エラー：『foobar』(識別子)が定義されていない。"),
            ("キーは、関数【入力がx,x】。辞書【「名前」が「ふぉーす」】のキーの値。",
             "エラー：「関数」は、「辞書」の要素の索引(ハッシュキー)として使用できない。"),
            ("甲は１。甲を空にし、甲を表示する。", "エラー：『甲』(識別子)が定義されていない。"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            XCTAssertEqual(evaluated.string, test.exptected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testDefineStatements() throws {
        let testPatterns: [(input: String, exptected: Int)] = [
            ("aは、5。a。", 5),
            ("aは、5と5を掛けたもの。a。", 25), ("aは、5。bは、a。b。", 5), ("aは、5。bは、a。cは、aとbと5を足したもの。c。", 15),
            ("『甲の乙』は、5。甲の乙。", 5),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.exptected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testFunctionObject() throws {
        let input = "関数であって、【入力がxであり、本体が、xに2を足す】。"
        print("テストパターン: \(input)")
        let function = try XCTUnwrap(testEvaluator(input) as? JpfFunction)
        let functionBlock = try XCTUnwrap(function.functions.array.first)
        XCTAssertEqual(functionBlock.parameters.count, 1)
        XCTAssertEqual(functionBlock.parameters.first?.string, "x")
        XCTAssertEqual(functionBlock.body?.string, "xに2を足す。")
        print("テスト(\(function.string))終了")
    }
    func testFunctionApplication() throws {
        let testPatterns: [(input: String, exptected: Any)] = [
            ("同一は、関数【入力がx、x】。５は同一である。", true),
            ("同一とは、関数【入力がx、xを返す】こと。５は同一である。", true),
            ("二倍とは、関数【入力がx、xに2を掛ける】こと。５を二倍し、二倍する。", 20),
            ("加えるとは、算出【入力がxとy、xとyを足す】こと。5に5を加える。", 10),
            ("加えるは、算出【入力がxとy、xとyを足す】こと。5と5を足したものに、5に5を加えたものを、加える。", 20),
            ("5で関数【入力がx、x】を実行する。", 5),
            ("加算は、関数【入力がaとb、aにbを足す】。適用は、関数【入力がaとbと演算、aとbを演算する】。2と2に、加算を適用する。", 4),
            ("減算は、関数【入力がaとb、aからbを引く】。適用は、関数【入力がaとbと演算、aとbを演算する】。10と2に、減算を適用する。", 8),
            ("正しいは、算出【入力がa、aが真である】。4が5より大きいは、正しくない。", true),
            ("『割った余り』は、関数【入力がxとy、xをyで割り、yを掛け、xから引いたものを返す】。23を11で割った余りは1である。", true),
            ("加算は、関数【入力がa「数値に」とb「数値を」、aにbを足し、返す】。1に2を加算する。", 3),
            ("加算は、関数【入力がa「数値」とb「数値」、aにbを足し、返す】。1と2の加算する。", 3),
            ("加算は、関数【入力がa「数値に」とb「数値を」、aにbを足し、返す】。「a」に「b」を加算する。", "入力の型が異なる。入力の型：文字列"),
            ("加算は、関数【入力がa「数値に」とb「数値を」、aにbを足し、返す】。1と2を加算する。", "入力の格が異なる。入力の格：と"),
            ("加算は、関数【入力がa「数値と…」とb「数値を」、aをbと関数【入力が初期値と値、初期値に値を足す】でまとめ、返す】。1と2と3と4と5を加算する。", 15),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.exptected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testClosure() throws {
        let input = """
        外側は、関数【
          カウント値は、０。
          内側は、関数【
            カウント値と1を足し、「カウント値」に上書きする。
            カウント値を返す。
          】。
          内側を返す。
        】
        外側を実行し、「カウントアップ」に代入する。
        """
        let testPatterns: [(input: String, expected: Int)] = [
            ("カウントアップする。", 1),
            ("カウントアップする。", 2),
            ("カウントアップする。", 3),
        ]
        print("テストパターン: \(input)")
        let environment = Environment()
        let parser = Parser(Lexer(input))
        let eval = Evaluator(from: parser.parseProgram()!, with: environment)
        let result = eval.object ?? environment.pull()
        XCTAssertFalse(result?.isError ?? false, result?.error?.message ?? "")
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let parser = Parser(Lexer(test.input))
            let eval = Evaluator(from: parser.parseProgram()!, with: environment)
            let result = try XCTUnwrap(eval.object ?? environment.pull())
            try testObject(result, with: test.expected)
            print("テスト結果(\(result.string))")
        }
        print("テスト終了")
    }
    func testOverloadedFunction() throws {
        let input = "加算は、さらに、関数【入力がa「数値に」とb「数値を」、aにbを足す】。加算。"
        print("テストパターン: \(input)")
        let function = try XCTUnwrap(testEvaluator(input) as? JpfFunction)
        let functionBlock = try XCTUnwrap(function.functions.array.first)
        XCTAssertEqual(functionBlock.parameters.count, 2)
        XCTAssertEqual(functionBlock.parameters[0].string, "a")
        XCTAssertEqual(functionBlock.parameters[1].string, "b")
        XCTAssertEqual(functionBlock.body?.string, "aにbを足す。")
        XCTAssertFalse(functionBlock.isOverloaded)
        print("テスト(\(function.string))終了")
    }
    func testOverloadedComputation() throws {
        let input = """
            加えるは、算出【入力がa「数値」とb「数値」、aにbを足し、返す】。
            加えるは、さらに、算出【入力がa「数値に」とb「数値を」、aにbを足し、返す】。
            加えるは、さらに、算出【入力がa「文字列」とb「文字列」、aにbを足し、返す】。
            識別子『加える』。
        """
        print("テストパターン: \(input)")
        let computation = try XCTUnwrap(testEvaluatorWithLabel(input) as? JpfComputation)
        XCTAssertTrue(computation.getters.hasRedefine)
        XCTAssertTrue(computation.setters.isEmpty)
        let functionBlocks = computation.getters.array
        XCTAssertEqual(functionBlocks.count, 3)
        XCTAssertEqual(functionBlocks[0].signature.numberOfInputs, 2)
        XCTAssertEqual(functionBlocks[0].signature.strings[0], "「数値」")
        XCTAssertEqual(functionBlocks[0].signature.strings[1], "「数値」")
        XCTAssertFalse(functionBlocks[0].isOverloaded)
        XCTAssertEqual(functionBlocks[1].signature.numberOfInputs, 2)
        XCTAssertEqual(functionBlocks[1].signature.strings[0], "「数値に」")
        XCTAssertEqual(functionBlocks[1].signature.strings[1], "「数値を」")
        XCTAssertTrue(functionBlocks[1].isOverloaded)
        XCTAssertEqual(functionBlocks[2].signature.numberOfInputs, 2)
        XCTAssertEqual(functionBlocks[2].signature.strings[0], "「文字列」")
        XCTAssertEqual(functionBlocks[2].signature.strings[1], "「文字列」")
        XCTAssertTrue(functionBlocks[2].isOverloaded)
        print("テスト(\(computation.string))終了")
    }
    func testRedefinedComputation() throws {
        let input = """
            加えるは、算出【取得が【入力がa「数値」とb「数値」、aにbを足し、返す】】。
            加えるは、さらに、算出【取得が、さらに、【入力がa「数値に」とb「数値を」、aにbを足し、返す】】。
            加えるは、さらに、算出【
                取得が、【入力がa「文字列」とb「文字列」、aにbを足し、返す】
                取得が、さらに、【入力がa「文字列に」とb「文字列を」、aにbを足し、返す】
            】。
            識別子『加える』。
        """
        print("テストパターン: \(input)")
        let computation = try XCTUnwrap(testEvaluatorWithLabel(input) as? JpfComputation)
        let functionBlocks = computation.getters.array
        XCTAssertEqual(functionBlocks.count, 2)
        XCTAssertEqual(functionBlocks[0].signature.numberOfInputs, 2)
        XCTAssertEqual(functionBlocks[0].signature.strings[0], "「文字列」")
        XCTAssertEqual(functionBlocks[0].signature.strings[1], "「文字列」")
        XCTAssertFalse(functionBlocks[0].isOverloaded)
        XCTAssertEqual(functionBlocks.count, 2)
        XCTAssertEqual(functionBlocks[1].signature.numberOfInputs, 2)
        XCTAssertEqual(functionBlocks[1].signature.strings[0], "「文字列に」")
        XCTAssertEqual(functionBlocks[1].signature.strings[1], "「文字列を」")
        XCTAssertTrue(functionBlocks[1].isOverloaded)
        print("テスト(\(computation.string))終了")
    }
    func testOverloadExecution() throws {
        let input = """
            加算は、関数【入力がa「文字列と…」とb「文字列を」、aの最初にbを足し、返す】。
            加算は、さらに、関数【入力がa「数値に」とb「数値を」、aにbを足し、返す】。
            加算は、さらに、関数【入力がa「文字列を」とb「文字列に」、bにaを足し、返す】。
            甲は、「あ」と「い」を加算したもの。
            乙は、1に2を加算したもの。
            丙は、「あ」を「い」に加算したもの。
            配列【甲、乙、丙】。
        """
        print("テストパターン: \(input)")
        let array = try XCTUnwrap(testEvaluator(input) as? JpfArray)
        XCTAssertEqual(array.elements.count, 3)
        XCTAssertEqual(array.elements[0].string, "あい")
        XCTAssertEqual(array.elements[1].string, "3")
        XCTAssertEqual(array.elements[2].string, "いあ")
        print("テスト(\(array.string))終了")
    }
    func testTypeObject() throws {
        let input = """
            型であって、【
                初期化は、【行列は、配列【】】。
                入れるは、関数で、入力が値、行列に値を追加し、「行列」に代入する。
                出すは、関数で、行列の先頭の値を積み、行列が空でない場合、行列の残りを「行列」に代入する。
            】
        """
        print("テストパターン: \(input)")
        let type = try XCTUnwrap(testEvaluator(input) as? JpfType)
        let initializer = try XCTUnwrap(type.initializers.array.first)
        XCTAssertEqual(initializer.parameters.count, 0)
        XCTAssertEqual(initializer.signature.numberOfInputs, 0)
        let initialization = try XCTUnwrap(initializer.body?.statements.first as? DefineStatement)
        XCTAssertEqual(initialization.name.value, "行列")
        XCTAssertEqual(initialization.value.expressions.count, 1)
        let array = try XCTUnwrap(initialization.value.expressions.first as? ArrayLiteral)
        XCTAssertTrue(array.elements.isEmpty)
        XCTAssertNotNil(type.body)
        XCTAssertEqual(type.body!.statements.count, 2)
        let first = try XCTUnwrap(type.body!.statements[0] as? DefineStatement)
        let second = try XCTUnwrap(type.body!.statements[1] as? DefineStatement)
        XCTAssertEqual(first.name.value, "入れる")
        XCTAssertEqual(second.name.value, "出す")
        _ = try XCTUnwrap(first.value.expressions.first as? FunctionLiteral)
        _ = try XCTUnwrap(second.value.expressions.first as? FunctionLiteral)
        print("テスト(\(type.string))終了")
    }
    func testTypeOperation() throws {
        let input = """
            自動車は、型であって、【
                型の要素が、【
                    ハンドルは、「右」。
                    切り替えるは、算出【ハンドルが「右」である場合【ハンドルに「左」を上書き】、それ以外は【ハンドルに「右」を上書き】】。
                】
                初期化が、【入力が残量、燃料量は、残量。色は「黒」。】
                給油は、関数であって、【入力が給油量で、
                    燃料量に給油量を足し、「燃料量」に上書きし、燃料量を返す。
                】
                「燃料量」と「給油」と「色」は利用可能。
            】
            自動車の要素「ハンドル」に「左」を設定する。
            黒い車は、10Lで自動車から生成する。
            30Lを黒い車に給油する。
            黒い車の要素「色」を「白」に設定する。
        """
        let testPatterns: [(input: String, expected: Any)] = [
            ("自動車の型。", "型"),
            ("黒い車の型。", "自動車"),
            ("自動車のハンドル", "左"),
            ("黒い車の(利用可能要素)数", 3),
            ("黒い車の燃料量。", 40),
            ("黒い車の色。", "白"),
            ("自動車を切り替えた自動車のハンドル", "右"),
        ]
        print("テストパターン: \(input)")
        let environment = Environment()
        let parser = Parser(Lexer(input))
        let eval = Evaluator(from: parser.parseProgram()!, with: environment)
        let result = eval.object ?? environment.pull()
        XCTAssertFalse(result?.isError ?? false, result?.error?.message ?? "")
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let parser = Parser(Lexer(test.input))
            let eval = Evaluator(from: parser.parseProgram()!, with: environment)
            let expected = eval.object ?? environment.pull()!
            try testObject(expected, with: test.expected)
            print("テスト(\(expected))終了")
        }
        print("テスト終了")
    }
    func testTypeInitOverloads() throws {
        let input = """
            甲は、型であって、【
                初期化は、【aは１。bは2。】
                初期化は、さらに、【入力がaで、bは3。】
                初期化は、さらに、【入力がaとb】
                「a」と「b」は利用可能。
            】
        """
        let testPatterns: [(input: String, expected: Int)] = [
            ("乙は甲から生成する。乙のaと乙のbを足す", 3),
            ("空にする。乙は、2で甲から生成する。乙のaと乙のbを足す", 5),
            ("空にする。乙は、3と4で甲から生成する。乙のaと乙のbを足す", 7),
            ("空にする。「乙」を甲から生成する。乙のaと乙のbを足す", 3),
            ("空にする。「乙」を、2で甲から生成する。乙のaと乙のbを足す", 5),
            ("空にする。「乙」を、3と4で甲から生成する。乙のaと乙のbを足す", 7),
            ("空にする。甲から「乙」を生成する。乙のaと乙のbを足す", 3),
            ("空にする。甲から、2で「乙」を生成する。乙のaと乙のbを足す", 5),
            ("空にする。甲から、3と4で「乙」を生成する。乙のaと乙のbを足す", 7),
        ]
        print("テストパターン: \(input)")
        let environment = Environment()
        let parser = Parser(Lexer(input))
        let eval = Evaluator(from: parser.parseProgram()!, with: environment)
        let result = eval.object ?? environment.pull()
        XCTAssertFalse(result?.isError ?? false, result?.error?.message ?? "")
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let parser = Parser(Lexer(test.input))
            let eval = Evaluator(from: parser.parseProgram()!, with: environment)
            let expected = eval.object ?? environment.pull()!
            try testObject(expected, with: test.expected)
            print("テスト(\(expected))終了")
        }
        print("テスト終了")
    }
    func testTypeInitExtensions() throws {
        let input = """
            甲は、型であって、【
                初期化は、【入力がaとbとc。】
                初期化は、さらに、【入力がa、aと1と2で、自身の初期化をする。】
                合計は、算出【aとbとcを足す。】
                「合計」は、利用可能。
            】
            甲は、さらに、型であって、【
                初期化は、さらに、【入力がaとb、aとbと1で、自身の初期化をする。】
            】
        """
        let testPatterns: [(input: String, expected: Int)] = [
            ("乙は、1と2と3で、甲から生成する。乙の合計", 6),
            ("空にする。乙は、1と2で、甲から生成する。乙の合計", 4),
            ("空にする。乙は、1で、甲から生成する。乙の合計", 4),
        ]
        print("テストパターン: \(input)")
        let environment = Environment()
        let parser = Parser(Lexer(input))
        let eval = Evaluator(from: parser.parseProgram()!, with: environment)
        let result = eval.object ?? environment.pull()
        XCTAssertFalse(result?.isError ?? false, result?.error?.message ?? "")
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let parser = Parser(Lexer(test.input))
            let eval = Evaluator(from: parser.parseProgram()!, with: environment)
            let expected = try XCTUnwrap(eval.object ?? environment.pull())
            try testObject(expected, with: test.expected)
            print("テスト(\(expected))終了")
        }
        print("テスト終了")
    }
    func testTypeExtension() throws {
        let input = """
            乙は、規約であって、条項が、型の丙は「数値」。
            甲は、型であって、【
                準拠する規約が、乙。
                型の要素が、【
                    丙は、１。
                    辛は、関数【入力がc、c】。
                】
                本体が、丁は、関数【入力がa、a】。
                「丁」は、利用可能。
            】
            戊は、規約であって、条項が、型の己は「文字列」。
            甲は、さらに、型であって、【
                準拠する規約が、戊。
                型の要素が、【
                    丙は、２。己は、「い」。
                    辛は、さらに、関数【入力がd、d】。
                】
                本体が、丁は、さらに、関数【入力がb、b】。
            】。
            庚は、甲から生成する。
        """
        let testPatterns: [(input: String, expected: Any)] = [
            ("甲の丙。", 2),
            ("甲の己。", "い"),
            ("空にする。4で、甲の辛を実行する。", 4),
            ("空にする。aは5。aで、庚の丁を実行する。", 5),
        ]
        print("テストパターン: \(input)")
        let environment = Environment()
        let parser = Parser(Lexer(input))
        let eval = Evaluator(from: parser.parseProgram()!, with: environment)
        let result = eval.object ?? environment.pull()
        XCTAssertFalse(result?.isError ?? false, result?.error?.message ?? "")
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let parser = Parser(Lexer(test.input))
            let eval = Evaluator(from: parser.parseProgram()!, with: environment)
            let result = try XCTUnwrap(eval.object ?? environment.pull())
            try testObject(result, with: test.expected)
            print("テスト(\(result))終了")
        }
        print("テスト終了")
    }
    func testProtocolOperation() throws {
        let input = """
            『挨拶の仕方』は、規約であって、挨拶は「関数」。
            『挨拶の仕方』は、さらに、型であって、挨拶は関数で、「こんにちは。」。
            『日中の挨拶』は、型であって、準拠する規約は、『挨拶の仕方』。
            『朝の挨拶』は、型であって、規約は、『挨拶の仕方』。挨拶は関数で、「おはよう。」。
        """
        let testPatterns: [(input: String, expected: Any)] = [
            ("『挨拶の仕方』の型。", "規約"),
            ("『日中の挨拶』の型。", "型"),
            ("『日中の挨拶』から生成した挨拶をする。", "こんにちは。"),
            ("『朝の挨拶』から生成した挨拶をする。", "おはよう。"),
        ]
        print("テストパターン: \(input)")
        let environment = Environment()
        let parser = Parser(Lexer(input))
        let eval = Evaluator(from: parser.parseProgram()!, with: environment)
        let result = eval.object ?? environment.pull()
        XCTAssertFalse(result?.isError ?? false, result?.error?.message ?? "")
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let parser = Parser(Lexer(test.input))
            let eval = Evaluator(from: parser.parseProgram()!, with: environment)
            let expected = eval.object ?? environment.pull()!
            try testObject(expected, with: test.expected)
            print("テスト(\(expected))終了")
        }
        print("テスト終了")
    }
    func testProtocolConformities() throws {
        let input = """
            甲は、規約であって、己は「数値」。
            乙は、規約であって、準拠する規約が、甲。
            丙は、規約であって、準拠する規約が、乙。条項が、庚は「文字列」。
            丁は、型であって、準拠する規約が、丙。本体が、己は１。庚は「い」。
            戊は、丁から生成する。
        """
        let testPatterns: [(input: String, expected: Any)] = [
            ("戊の己。", 1),
            ("戊の庚。", "い"),
        ]
        print("テストパターン: \(input)")
        let environment = Environment()
        let parser = Parser(Lexer(input))
        let eval = Evaluator(from: parser.parseProgram()!, with: environment)
        let result = eval.object ?? environment.pull()
        XCTAssertFalse(result?.isError ?? false, result?.error?.message ?? "")
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let parser = Parser(Lexer(test.input))
            let eval = Evaluator(from: parser.parseProgram()!, with: environment)
            let result = try XCTUnwrap(eval.object ?? environment.pull())
            try testObject(result, with: test.expected)
            print("テスト(\(result))終了")
        }
        print("テスト終了")
    }
    func testParameterConformity() throws {
        let input = """
            <規約>は、規約であって、【
                <関数>は、関数【入力がa「数値」】
                <取得>は、算出【入力がb「文字列」】
                <設定>は、算出【
                    設定は、【入力がc「配列」】
                】
                <算出>は、算出【
                    設定は、【入力がd「と」】
                    取得は、【入力がe「で」】
                】
            】
            <型>は、型【
                準拠する規約が<規約>。
                本体が、【
                    <関数>は、関数【入力がa「数値」。aを返す。】
                    <関数>は、さらに、関数【１を返す。】
                    <取得>は、算出【入力がb「数値」。bを返す。】
                    <取得>は、さらに、算出【入力がb「文字列」。bを返す。】
                    <設定>は、算出【
                        設定は、【入力がc「配列」。cの最初を返す。】
                    】
                    <算出>は、算出【
                        設定は、【入力がd「と」。dを返す。】
                        取得は、【入力がe「で」。eを返す。】
                    】
                】
            】
            <インスタンス>は、<型>から生成する。
            <インスタンス>。
        """
        print("テストパターン: \(input)")
        let result = try XCTUnwrap(testEvaluator(input))
        if result.isError {print("テスト失敗：\(result.string)")}
        print("<インスタンス>は、\(result.string)")
        print("テスト終了")
    }
    func testBuiltinOperations() throws {
        let testPatterns: [(input: String, exptected: Any?)] = [
            ("「こんにちは」の型","文字列"),
            ("cは、型【aは1】。iはcから生成する。iの型", "c"),
            ("１を積む。得たものの格", nil),
            ("aは１。aの格", nil),
            ("「い」を積む。得たものの値", "い"),
            ("aは「い」。aの値", "い"),
            ("aは１。aの数値", 1),
            ("aは「い」。aの数値", nil),
            ("aは１。aの文字列", "1"),
            ("aは「い」。aの文字列", "い"),
            ("aは１。aの識別子名", "a"),
            ("aは１。aを積む。得たものの値の識別子名", "a"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.exptected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testStringBuiltins() throws {
        let testPatterns: [(input: String, exptected: String?)] = [
            ("「こんにちは」と「、」と「みなさん。」を足す。","こんにちは、みなさん。"),
            ("「あいうえお」の先頭", "あ"),
            ("「あいうえお」の後尾", "お"),
            ("「あいうえお」の２番目", "う"),
            ("「あいうえお」の残り", "いうえお"),
            ("「あいうえお」を逆順にする。", "おえういあ"),
            ("「」の最初", nil), ("「」の最後", nil), ("「」の２番目", nil), ("「」の残り", nil),
            ("「あいうえお」の範囲【1から3まで】", "いうえ"),
            ("「あいうえお」の範囲【1以上3以下】", "いうえ"),
            ("「あいうえお」の範囲【1以上3未満】", "いう"),
            ("「あいうえお」の範囲【3から】", "えお"),
            ("「あいうえお」の範囲【2まで】", "あいう"),
            ("「あいうえお」の範囲【2未満】", "あい"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.exptected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testBuiltinFunctions() throws {
        let testPatterns: [(input: String, exptected: Any)] = [
            // の数
            ("「」の数", 0), ("「four」の数", 4), ("「こんにちは」の数", 5),
            ("１の数", "数値型の要素の数は、数えることができない。"),
            ("「one」と「two」を足したものの数", 6),
            // が空
            ("１が空", "数値型の要素の数は、数えることができない。"),
            ("「」が空", true), ("「four」が空", false),
            ("範囲【1から3まで】が空", false), ("範囲【1以上1未満】が空", true),
            ("aは配列。aが空", true), ("配列【1, 2】が空", false),
            ("dは辞書。dが空", true), ("辞書【「一」が1, 「二」が2】が空", false),
            ("eは列挙。eが空", true), ("列挙【a, b】が空", false),
            // 含む
            ("「かきくけこ」が「く」を含む。", true), ("「なにぬねの」が「く」を含まない。", true),
            ("範囲【1以上5未満】が5を含む", false), ("範囲【1以上】が3を含む", true),
            ("範囲【1まで】が3を含む", false), ("範囲【3未満】が3を含む", false),
            ("配列【１、２、３、４、５】が3を含む", true), 
            ("配列【１、２、３、４、５】が配列【１、３】を含む", false),
            ("配列【１、２、３、４、配列【１、３】】が配列【１、３】を含む", true),
            ("配列【１、２、３、４、５】が範囲【６以上】を含む", false),
            ("配列【１、２0、３、４0、５】が、関数【入力が数値、数値が10より大きい】を含む", true),
            ("辞書【「あ」が１、「い」が２、「う」が30】が、関数【入力がキーと値、値が10より大きい】を含む", true),
            // 並べ替える
            ("配列【３、１、４、２、５】を並べ替える。", [1,2,3,4,5]), ("配列【３、１、４、２、５】を「昇順」に並べ替える。", [1,2,3,4,5]),
            ("配列【３、１、４、２、５】を「降順」に並べ替える。", [5,4,3,2,1]),
            ("配列【３、１、４、２、５】を、関数【入力が甲と乙、甲が乙より大きい】で並べ替える。", [5,4,3,2,1]),
            // まとめる
            ("配列【「あ」、「い」、「う」、「え」、「お」】を、「」と関数【入力が初期値と値、初期値に値を足す】でまとめる。", "あいうえお"),
            ("辞書【１が「１」、２が「２」、３が「３」、４が「４」、５が「５」】を、0と関数【入力が初期値とキーと値、初期値にキーを足す】でまとめる。", 15),
            ("範囲【1から5まで】を、０と関数【入力が初期値と数値、初期値に数値を足す】でまとめる。", 15),
            ("範囲【1以上6未満】を、０と関数【入力が初期値と数値、初期値に数値を足す】でまとめる。", 15),
            // 写像する
            ("配列【１、２、３、４、５】を関数【入力が数、数に2を掛ける】で写像する。", [2,4,6,8,10]),
            ("辞書【１が「１」、２が「２」、３が「３」、４が「４」、５が「５」】を、関数【入力がキーと値、キー】で写像し、並べ替える。", [1,2,3,4,5]),
            ("範囲【1から5まで】を、関数【入力が数値、数値を返す】で写像する。", [1,2,3,4,5]),
            ("範囲【1以上6未満】を、関数【入力が数値、数値】で写像する。", [1,2,3,4,5]),
            ("範囲【1以上5以下】を写像する。", [1,2,3,4,5]),
            ("範囲【1以上6未満】を写像する。", [1,2,3,4,5]),
            // 絞り込む
            ("配列【１、２0、３、４0、５】を、関数【入力が数値、数値が10より大きい】で絞り込む。", [20,40]),
            ("辞書【「あ」が１、「い」が２、「う」が30】を、関数【入力がキーと値、値が10より大きい】で絞り込み、関数【入力がキーと値、値】で写像する。", [30]),
            // 逆順にする
            ("配列【３、１、４、２、５】を逆順にする。", [5,2,4,1,3]),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            if let error = evaluated as? JpfError {
                XCTAssertEqual(error.message, test.exptected as? String)
            } else {
                try testObject(evaluated, with: test.exptected)
            }
            print("テスト(\(evaluated))終了")
        }
    }
    func testArrayLiteral() throws {
        let input = "配列【1、2と2を掛ける、3と3を足す】。"
        print("テストパターン: \(input)")
        let evaluated = try XCTUnwrap(testEvaluator(input) as? JpfArray)
        XCTAssertEqual(evaluated.elements.count, 3)
        try testObject(evaluated.elements[0], with: 1)
        try testObject(evaluated.elements[1], with: 4)
        try testObject(evaluated.elements[2], with: 6)
        print("テスト(\(evaluated.string))終了")
    }
    func testArrayInitialization() throws {
        let input = "配列であって、要素が、3個の３。"
        print("テストパターン: \(input)")
        let evaluated = try XCTUnwrap(testEvaluator(input) as? JpfArray)
        XCTAssertEqual(evaluated.elements.count, 3)
        try testObject(evaluated.elements[0], with: 3)
        try testObject(evaluated.elements[1], with: 3)
        try testObject(evaluated.elements[2], with: 3)
        print("テスト(\(evaluated.string))終了")
    }
    func testArrayIndexExpressions() throws {
        let testPatterns: [(input: String, expected: Int?)] = [
            ("配列【1, 2, 3】の0番目", 1), ("配列【1, 2, 3】の1番目", 2), ("配列【1, 2, 3】の2番目", 3),
            ("iは、0。配列【1】のi。", 1), ("『位置』は、1と1を足したもの。配列【1, 2, 3】の『位置』。", 3),
            ("myArrayは、配列【1, 2, 3】。myArrayの2番目。", 3), ("myArrayは、配列【1, 2, 3】。myArrayの0番目と、myArrayの1番目と、myArrayの2番目を足す。", 6), ("myArrayは、配列【1, 2, 3】。iは、myArrayの0番目。myArrayのi", 2),
            ("配列【1, 2, 3】の3番目", nil), ("配列【1, 2, 3】の-1番目", nil),
            ("配列【1, 2, 3】の範囲【0から1まで】の数", 2),
            ("配列【1, 2, 3】の範囲【2未満】の数", 2),
            ("甲は範囲【2以上】。配列【1, 2, 3】の甲の数", 1),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testArrayAppendeds() throws {
        let testPatterns: [(input: String, expected: Int?)] = [
            ("aは、配列【1, 2, 3】に４を追加したもの。aの後尾", 4),
            ("aは、4を配列【1, 2, 3】に追加したもの。aの後尾", 4),
            ("配列【1, 2, 3】。4を追加したものの最後", 4),
            ("配列【1, 2, 3】に配列【４、５、６】を足したものの後尾。", 6),
            ("aは、配列【1, 2, 3】に４を追加したもの。aの4番目", nil),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testArrayDeleteds() throws {
        let testPatterns: [(input: String, expected: Int?)] = [
            ("aは、配列【1, 2, 3】から、２番目を削除したもの。aの後尾", 2),
            ("配列【1, 2, 3】。０番目を削除したものの先頭", 2),
            ("配列【1, 2, 3】から３番目を削除したものの後尾。", 3),
            ("配列【1, 2, 3】から「先頭」を削除したものの先頭。", 2),
            ("配列【1, 2, 3】から「後尾」を削除したものの後尾。", 2),
            ("配列【1, 2, 3】から「全て」を削除したものの数", 0),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testDictionaryLiteral() throws {
        let input = """
            辞書【要素が、
                「その一」が、10から9を引いたもの、
                「その二」が、1と1を足したもの、
                「その」と「三」を足したものが、6を2で割ったもの、
                4が4、
                真が5、
                偽が6
            】
        """
        print("テストパターン: \(input)")
        let evaluated = try XCTUnwrap(testEvaluator(input))
        let result = try XCTUnwrap(evaluated as? JpfDictionary)
        let expected: [JpfHashKey: Int] = [
            JpfString(value: "その一").hashKey: 1,
            JpfString(value: "その二").hashKey: 2,
            JpfString(value: "その三").hashKey: 3,
            JpfInteger(value: 4).hashKey:      4,
            JpfBoolean.TRUE.hashKey:           5,
            JpfBoolean.FALSE.hashKey:          6,
        ]
        XCTAssertEqual(result.pairs.count, expected.count)
        for (expectedKey, expectedValue) in expected {
            let pair = try XCTUnwrap(result.pairs[expectedKey])
            try testObject(pair.value, with: expectedValue)
        }
        print("テスト(\(evaluated.string))終了")
    }
    func testDitctionaryIndexExpressions() throws {
        let testPatterns: [(input: String, expected: Any?)] = [
            ("辞書【「索引」が５】の「索引」", 5),
            ("辞書【「索引」が５】の「キー」", nil),
            ("『要素』は、「キー」。辞書【「キー」が５】の『要素』", 5),
            ("辞書【】の「索引」の値", nil),
            ("辞書【5が5】の5の値", 5),
            ("辞書【真が5】の真の値", 5),
            ("辞書【偽が５】の偽の値", 5),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testDictionaryAppendeds() throws {
        let testPatterns: [(input: String, expected: Int?)] = [
            ("dは、辞書【「その一」が１】に「その二」が２を追加したもの。dの「その二」", 2),
            ("dは、「その三」が３を、辞書【「その一」が１】に追加したもの。dの「その三」", 3),
            ("dは、辞書【「その一」が１】に辞書【「その二」が2】を追加したもの。dの「その二」", 2),
            ("dは、辞書【「その三」が３】を、辞書【「その一」が１】に追加したもの。dの「その三」", 3),
            ("dは、辞書【「その一」が１】に「その一」が２を追加したもの。dの「その一」", 2),
            ("dは、辞書【「その一」が１、「その二」が2】に辞書【「その一」が３】を追加したもの。dの「その一」", 3),
            ("dは、辞書【「その一」が１】に「その二」が２を追加したもの。dの「その三」", nil),
            ("辞書【「その一」が１】。「その四」が4を追加したものの「その四」", 4),
            ("辞書【「その一」が１】。辞書【「その四」が4】を追加したものの「その四」", 4),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testDictionaryDeleteds() throws {
        let testPatterns: [(input: String, expected: Int?)] = [
            ("辞書【「その一」が１、「その二」が２、「その三」が３】から、「その二」を削除したものの「その二」", nil),
            ("辞書【「その一」が１、「その二」が２、「その三」が３】から、「全て」を削除したものの数", 0),
            ("辞書【「その一」が１、「その二」が２、「その三」が３】から、「その四」を削除したものの数", 3),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testOrPhrase() throws {
        let input = "「その一」または「その二」または「その三」に"
        print("テストパターン: \(input)")
        let expecteds = ["その一", "その二", "その三"]
        let evaluated = try XCTUnwrap(testEvaluator(input))
        let phrase = try XCTUnwrap(evaluated as? JpfPhrase)
        XCTAssertEqual(phrase.particle, .particle(.NI))
        let array = try XCTUnwrap(phrase.value as? JpfArray)
        for (element, expected) in zip(array.elements, expecteds) {
            let string = try XCTUnwrap(element as? JpfString)
            try testObject(string.value!, with: expected)
        }
        print("テスト(\(evaluated))終了")
    }
    func testAsWellAsPredicate() throws {
        let input = "１を2から引いたもの、および3と4と５を足したもので、関数【入力がaとb、bからaを引く】を実行。"
        print("テストパターン: \(input)")
        let evaluated = try XCTUnwrap(testEvaluator(input))
        XCTAssertEqual(evaluated.number, 11)
        print("テスト(\(evaluated))終了")
    }
    func testSelectiveOrEvaluations() throws {
        let testPatterns: [(input: String, expected: Bool)] = [
            ("指定値は、「その二」。指定値が「その一」または「その二」または「その三」である", true),
            ("指定値は、「その四」。指定値が「その一」または「その二」または「その三」である", false),
            ("指定値は、「その四」。指定値が「その一」または「その二」または「その三」ではない", true),
            ("指定値は、「その二」。指定値が「その一」または「その二」または「その三」に等しい", true),
            ("指定値は、「その四」。指定値が「その一」または「その二」または「その三」に等しくない", true),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testLogicalOperations() throws {
        let testPatterns: [(input: String, expected: Bool)] = [
            ("1が2に等しい、または、1が1に等しい", true),
            ("1が2に等しい、または、1が3に等しい", false),
            ("1が2に等しい、かつ、1が1に等しい", false),
            ("1が1に等しい、かつ、2が2に等しい、かつ、3が3に等しい", true),
            ("1が1に等しい、または、2が2に等しい、または、3が3に等しい", true),
            ("1が1に等しい、または、【2が2に等しい】、かつ、3が4に等しい", false),
            ("1が1に等しい、または、2が2に等しい、かつ、3が4に等しい", false),
            ("1が1に等しい、または、【2が2に等しい、かつ、3が4に等しい】", true),
            ("1が2に等しい、かつ、2が2に等しい、または、3が3に等しい", true),
            ("1が2に等しい、かつ、【2が2に等しい】、または、3が3に等しい", true),
            ("1が2に等しい、かつ、【2が2に等しい、または、3が3に等しい】", false),
            ("1が1または2に等しい、かつ、2が2に等しい、または、3が3に等しい", true),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testLoopOperations() throws {
        let testPatterns: [(input: String, expected: Int)] = [
            ("合計は０。1から９まで反復【入力は数字、合計は、合計に数字を足したもの】。合計。", 45),
            ("合計は０。範囲【1から９まで】を反復【入力は数字、合計は、合計に数字を足したもの】。合計。", 45),
            ("合計は０。範囲【1以上10未満】を反復【入力は数字、合計は、合計に数字を足したもの】。合計。", 45),
            ("合計は０。9から１まで−1ずつ反復【入力は数字、合計は、合計に数字を足したもの】。合計。", 45),
            ("合計は０。配列【１、２、３、４、５、６、７、８、９】を反復【入力は数字、合計は、合計に数字を足したもの】。合計。", 45),
            ("合計は０。辞書【1が１、2が２、3が３、4が４、5が５、6が６、7が７、8が８、9が９】を反復【入力が索引と値、合計は、合計に値を足す】。合計。", 45),
            ("合計は０。数字は1。反復であって、条件が、数字が10より小さい間、合計は、合計に数字を足したもの。数字は、数字に１を足したもの。合計。", 45),
            ("""
            合計は０。数字は1。
            反復【
                合計は、合計に数字を足したもの。
                数字は、数字に１を足したもの。
                数字が９より大きい場合、中止する。
            】。
            合計。
            """, 45),
            ("合計は０。範囲【1から９まで】で関数【入力は数字、数字と】を繰り返す。足す。", 45),
            ("合計は０。範囲【1以上10未満】で関数【入力は数字、数字と】を繰り返す。足す。", 45),
            ("合計は０。配列【１、２、３、４、５、６、７、８、９】で関数【入力は数字、数字と】を繰り返す。足す。", 45),
            ("合計は０。辞書【1が１、2が２、3が３、4が４、5が５、6が６、7が７、8が８、9が９】で関数【入力は索引と値、値と】を繰り返す。足す。", 45),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testRangeCheckings() throws {
        let testPatterns: [(input: String, expected: Bool)] = [
            ("1が範囲【１以下】にある", true),
            ("1が範囲【１未満】にある", false),
            ("2が範囲【１以上】にある", true),
            ("1が範囲【１以上２以下】にある", true),
            ("2が範囲【１以上3未満】にある", true),
            ("2が範囲【１以上3未満】にない", false),
            ("1が範囲【１から10まで】にある", true),
            ("11が範囲【１から10まで】にある", false),
            ("3が範囲【１から2まで】または範囲【４から10まで】にある", false),
            ("範囲【1から3まで】は範囲【1以上3以下】に等しい", true),
            ("範囲【1から3まで】は範囲【1以上3未満】に等しい", false),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testStackOperations() throws {
        let testPatterns: [(input: String, expected: Any?)] = [
            ("1を積む。甲は写したもの。甲と得たものを足す。", 2),
            ("2を積む。「甲」に得る。甲の値。", 2),
            ("3を積み、捨てる。甲は写したもの。甲。", nil),
            ("4と５を積み、１個捨てる。甲は得たもの。甲の値。", 4),
            ("6と7を積み、２個捨てる。入力が空。", true),
            ("8と9を積み、空にする。入力が空", true),
            ("10と11と12を積む。「甲」に３個写す。空にする。甲の最後の値。", 12),
            ("13と14と15を積む。「甲」に３個得る。甲の最初の値。", 13),
            ("16を積む。甲は3個得たもの。甲", nil),
            ("17を積む。「甲」に「数値」を得る。甲の値。", 17),
            ("「a」を積む。甲は「数値」を得たもの。甲。", nil),
            ("「b」と「c」と「d」を積む。「甲」に「数値」を3個得る。甲。", nil),
            ("18と19と20を積む。「甲」に「数値」を3個得る。甲の１番目の値。", 19),
            ("21と「a」を積む。捨てる。甲は1個得たもの。甲の格。", "と"),
            ("22と「a」を積む。捨てる。甲は「値」を1個得たもの。甲の格。", nil),
            ("23と24を積む。「甲」と「乙」に得る。甲に乙を足す。", 47),
            ("25と26を積む。「甲」と「乙」に写す。甲から乙を引く。", -1),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testAssignAndSwapOperations() throws {
        let testPatterns: [(input: String, expected: Any?)] = [
            ("1を「甲」に代入する。甲。", 1),
            ("1と2を足す。「甲」に代入する。甲。", 3),
            ("甲は1。甲に「い」を代入する。甲。", "い"),
            ("甲は１。乙は２。甲と乙を入れ替える。甲", 2),
            ("甲は3。乙は4。「甲」と「乙」を入れ替える。甲", 4),
            ("１と２を入れ替える。入力の最初の値", 2),
            ("３と４を入れ替える。入力の１の値", 3),
            ("５と６を入れ替える。入力の後尾の値", 5),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testOverwriteOperation() throws {
        let input = """
            キュー(FIFO)は、配列。
            入れるは、算出で、入力が値、キューに値を追加して、上書きする。
            取り出すは、算出で、キューの先頭を積み、キューが空でない場合、キューの残りをキューに上書きする。
            3を入れる。
            5を入れる。
            取り出す。
        """
        print("テストパターン: \(input)")
        let evaluated = try XCTUnwrap(testEvaluator(input) as? JpfInteger)
        XCTAssertEqual(evaluated.value, 3)
        print("テスト(\(evaluated))終了")
    }
    func testAssignElements() throws {
        let testPatterns: [(input: String, expected: Any?)] = [
            ("配列【１、２、３】の位置１に５を代入したものの１。", 5),
            ("甲は、配列【１、２、３】。乙は０。甲の位置「乙」に５を代入する。甲の最初", 5),
            ("甲は、配列【１、２、３】。関数【５を甲の位置２に上書きする。】を実行する。甲の最後", 5),
            ("甲は、配列【１、２、３】。乙は２。関数【５を甲の位置『乙』に上書きする。】を実行する。甲の最後", 5),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testCompoundAssignmentOperations() throws {
        let testPatterns: [(input: String, expected: Any?)] = [
            ("xは１。xに1を足して代入。x", 2),
            ("xは１。xから1を引いて代入。x", 0),
            ("xは１。xを3から引いて代入。x", 2),
            ("xは２。xに2を掛けて代入。x", 4),
            ("xは４。xを2で割って代入。x", 2),
            ("xは２。xで6を割って代入。x", 3),
            ("xは１。xを負数にして代入。x", -1),
            ("xは配列【１、２】。xに３を追加して代入。xの最後", 3),
            ("xは配列【１、２】。xから１番目を削除して代入。xの最後", 1),
            ("xは辞書【「い」が1】。xに「ろ」が2を追加して代入。xの数", 2),
            ("xは辞書【「い」が1、「ろ」が2】。xから「い」を削除して、代入。xの数", 1),
            ("xは「いろ」。xに「は」を足して代入。x", "いろは"),
            ("xは「あいうえお」。xを逆順にして代入。x", "おえういあ"),
            ("xは２。xに2を掛け、3を足し、4を引いて代入する。x", 3),
            ("xは１。xに1を足し代入。x", "仕様：<識別子>(を)<計算し>て代入する。"),
            ("""
            xは１。
            yは、関数【xに1を足して上書き。】
            yを実行する。
            x
            """, 2),
            ("""
            yは、関数【xは1。xに1を足して上書き。】
            yを実行する。
            """, "エラー：『x』(識別子)が定義されていない。"),
            ("fは関数【入力がa、aを返す】。xは１。xでfを実行して代入する。", "エラー：代入先の識別子が不明。仕様：<識別子>(を)<計算し>て代入する。"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testLabelExpressions() throws {
        let testPatterns: [(input: String, expected: Any)] = [
            ("二倍は２。識別子「二倍」。『識別子』。", "二倍"),
            ("『割った余り』は１。識別子『割った余り』。『識別子』。", "割った余り"),
            ("ファイル「サンプル」。『ファイル』。", "サンプル"),
            ("位置１。", 1), ("位置「１」。", 1),
            ("甲は１。位置「甲」。", 1),
            ("甲は１。位置『甲』。", 1),
            ("甲は１。位置 甲。", 1),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let evaluated = try XCTUnwrap(testEvaluator(test.input))
            try testObject(evaluated, with: test.expected)
            print("テスト(\(evaluated))終了")
        }
    }
    func testEnumOperations() throws {
        let input = """
            オプションは、列挙【無し、一、二と、未定義は999】。
            曜日は、列挙であって、【
                要素が、
                    月は「月曜」と、火は「火曜」、水は「水曜」、木は「木曜」、金は「金曜」、土は「土曜」、日は「日曜」。
            】。
        """
        let testPatterns: [(input: String, expected: Any)] = [
            ("オプションの型。", "列挙"),
            ("オプションの数。", 4),
            ("オプションの無しの値。", 0),
            ("オプションの二の列挙子。", "二"),
            ("オプションの二の値。", 2),
            ("オプションの未定義の値。", 999),
            ("オプションの列挙子の最初", "無し"),
            ("曜日の型。", "列挙"),
            ("曜日の数。", 7),
            ("日曜日は、曜日の日。日曜日の列挙子。", "日"),
            ("日曜日は、曜日の日。日曜日の型。", "曜日"),
            ("列挙【月、火、水、木、金、土、日】の日の型。", ""),
            ("日曜日は、曜日・日。日曜日の値。", "日曜"),
            ("月曜日は、曜日・月。月曜日が・月である。", true),
            ("今日は、曜日・火。今日が、・火の場合、【１】、それ以外は、【２】。", 1),
            ("予定日は、「水曜」で曜日から生成したもの。予定日の列挙子。", "水"),
            ("「月曜」で曜日から「月曜日」を生成する。月曜日の列挙子。", "月"),
            ("「Monday」を月曜日の値で曜日から生成する。Mondayの列挙子。", "月"),
            ("木曜日は、「Thursday」を・木に代入したもの。木曜日の値。", "Thursday"),
        ]
        print("テストパターン: \(input)")
        let environment = Environment()
        let parser = Parser(Lexer(input))
        let eval = Evaluator(from: parser.parseProgram()!, with: environment)
        let result = eval.object ?? environment.pull()
        XCTAssertFalse(result?.isError ?? false, result?.error?.message ?? "")
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let parser = Parser(Lexer(test.input))
            let eval = Evaluator(from: parser.parseProgram()!, with: environment)
            let expected = eval.object ?? environment.pull()!
            try testObject(expected, with: test.expected)
            print("テスト(\(expected))終了")
        }
        print("テスト終了")
    }
    func testComputations() throws {
        let input = """
            挨拶は、型であって、【
                型の要素が、【宛名は、「田中」。本文は、算出【「こんにちは、」と宛名と「さん。」を足す】】。
            】。
            温度は、型であって、【
                摂氏は、0。
                華氏は、算出【
                    設定が、【入力が設定値、設定値から３２を引いたものに５を掛け、９で割り、「摂氏」に上書きする。】
                    取得が、【９に摂氏を掛け５で割ったものに、３２を足す。】
                】。
                「摂氏」と「華氏」は利用可能。
            】。
            気温は、温度から生成する。
            税込は、算出であって、入力が金額、金額を10で割って、金額を足す。
        """
        let testPatterns: [(input: String, expected: Any)] = [
            ("挨拶の本文。", "こんにちは、田中さん。"),
            ("気温の摂氏。", 0),
            ("気温の華氏。", 32),
            ("気温の要素「摂氏」に20度を設定する。気温の摂氏。", 20),
            ("気温の華氏。", 68),
            ("気温の要素「華氏」を32度に設定する。気温の摂氏。", 0),
            ("気温の華氏。", 32),
            ("空にする。100円の税込。", 110),
        ]
        print("テストパターン: \(input)")
        let environment = Environment()
        let parser = Parser(Lexer(input))
        let eval = Evaluator(from: parser.parseProgram()!, with: environment)
        let result = eval.object ?? environment.pull()
        XCTAssertFalse(result?.isError ?? false, result?.error?.message ?? "")
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let parser = Parser(Lexer(test.input))
            let eval = Evaluator(from: parser.parseProgram()!, with: environment)
            let expected = try XCTUnwrap(eval.object ?? environment.pull())
            try testObject(expected, with: test.expected)
            print("テスト(\(expected))終了")
        }
        print("テスト終了")
    }
    func testComputationOverloads() throws {
        let input = """
            甲は、型であって、【
                乙は、算出【
                    設定が、【入力がa、丁にaを上書き。】
                    設定が、さらに、【入力がb、丁にbを上書き。】
                    取得が、【１】
                    取得が、さらに、【入力がc、c】
                】
                丁は、0。
                「乙」と「丁」は利用可能。
            】。
            丙は、甲から生成する。
        """
        let testPatterns: [(input: String, expected: Any)] = [
            ("丙の乙", 1),
            ("空にする。cは2。cで丙の乙", 2),
            ("空にする。aは3。丙の要素「乙」にaを設定する。丙の丁。", 3),
            ("空にする。bは4。丙の要素「乙」にbを設定する。丙の丁。", 4),
        ]
        print("テストパターン: \(input)")
        let environment = Environment()
        let parser = Parser(Lexer(input))
        let eval = Evaluator(from: parser.parseProgram()!, with: environment)
        let result = eval.object ?? environment.pull()
        XCTAssertFalse(result?.isError ?? false, result?.error?.message ?? "")
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let parser = Parser(Lexer(test.input))
            let eval = Evaluator(from: parser.parseProgram()!, with: environment)
            let expected = try XCTUnwrap(eval.object ?? environment.pull())
            try testObject(expected, with: test.expected)
            print("テスト(\(expected))終了")
        }
        print("テスト終了")
    }
    func testOutputs() throws {
        struct TestOperator : PredicateOperable {
            init(_ environment: Environment) {self.environment = environment}
            let environment: Environment
            func operated() -> (any JpfObject)? {
                var result = OutputString()
                guard let object = leftOperand else {return nil}
                if let error = output(object, withEscapeProcessing: true,
                                      out: {result.concat($0,$1)}) {return error}
                return JpfString(value: result.string)
            }
        }
        struct OutputString {
            var string = ""
            mutating func concat(_ s1: String, _ s2: String?) {
                string += s1 + (s2 ?? "")
            }
        }
        let testPatterns: [(input: String, expected: String)] = [
            ("「みなさん、こんにちは。」", "みなさん、こんにちは。"),
            ("「みなさん、」に「こんにちは。」を足す。", "みなさん、こんにちは。"),
            ("「こんにちは、\\改行なし」", "こんにちは、"),
            ("「い\\nろ\\nは」", "い\nろ\nは"),
            ("「あ\\末尾\\t」と「いうえお」を足す。", "あ\tいうえお"),
            ("「かぎ括弧\\「\\」」", "かぎ括弧「」"),
            ("甲は１。「答えは、\\『甲』。」", "答えは、1。"),
            ("甲は関数【「何もしない」】。「甲は、\\(甲)。」",
             "甲は、関数であって、【本体が、「何もしない」】。"),
            ("二倍は、関数【入力がx、xに2を掛ける】。二倍",
             "関数であって、【入力が、xであり、本体が、xに2を掛ける】"),
            ("加えるは、算出【取得が【入力がa「数値」とb「数値」、aにbを足し、返す】】。識別子「加える」",
             "算出であって、【取得は、【入力が、a「数値」とb「数値」であり、本体が、aにbを足し、返す】】"),
            ("加えるは、算出【取得が【入力がa「数値」とb「数値」、aにbを足し、返す】】。「『加える』は、\\『加える』」",
             "『加える』は、算出であって、【取得は、【入力が、a「数値」とb「数値」であり、本体が、aにbを足し、返す】】"),
            ("文字列は、「い\\nろ\\nは」。識別子「文字列」", "い\\nろ\\nは"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let lexer = Lexer(test.input)
            let parser = Parser(lexer)
            guard let program = parser.parseProgram(), parser.errors.isEmpty else {
                parser.errors.forEach {print("Parser errors: \($0)")}
                XCTFail()
                return
            }
            let environment = Environment()
            let evaluated = Evaluator(from: program, with: environment).object
            XCTAssertFalse(evaluated != nil && evaluated!.isError, evaluated?.string ?? "nil")
            let testOperator = TestOperator(environment)
            let result = try XCTUnwrap(testOperator.operated())
            XCTAssertEqual(result.string, test.expected)
            print("テスト(\(result.string))終了")
        }
    }
}
// MARK: - ヘルパー
private func testObject(_ object: JpfObject, with exptected: Int) throws {
    let result = try XCTUnwrap(object as? JpfInteger, "object is \(object)")
    XCTAssertEqual(result.value, exptected, "期待値は\(exptected)だが、実値は\(result.value)")
}
private func testObject(_ object: JpfObject, with exptected: Bool) throws {
    let result = try XCTUnwrap(object as? JpfBoolean, "object is \(object)")
    XCTAssertEqual(result.value, exptected, "期待値は\(exptected)だが、実値は\(result.value)")
}
private func testObject(_ object: JpfObject, with exptected: Any?) throws {
    switch exptected {
    case let int as Int:
        try testObject(object, with: int)
    case let boolean as Bool:
        try testObject(object, with: boolean)
    case let array as [Int]:
        let arrayObject = try XCTUnwrap(object as? JpfArray)
        for (object, int) in zip(arrayObject.elements, array) {
            try testObject(object, with: int)
        }
    case let string as String:
        var result = ""
        switch object {
        case let s as JpfString:    result = s.value
        case let e as JpfError:     result = e.message
        default:                    XCTFail("object is \(object)")
        }
        XCTAssertEqual(result, string)
    case nil:
        XCTAssertTrue(object.isNull, "評価結果が「無」でなかった。")
    default:
        XCTFail("テスト値の型\(type(of: exptected))は未サポート")
    }
}
private func testEvaluatorWithLabel(_ input: String) -> JpfObject? {
    let lexer = Lexer(input)
    let parser = Parser(lexer)
    guard let program = parser.parseProgram(), parser.errors.isEmpty else {
        parser.errors.forEach {print("Parser errors: \($0)")}
        return nil
    }
    let environment = Environment()
    let eval = Evaluator(from: program, with: environment)
    guard let label = (eval.object ?? environment.peek) as? JpfString else {return nil}
    return environment[label.value]
}
