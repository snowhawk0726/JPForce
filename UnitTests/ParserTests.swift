//
//  ParserTests.swift
//  UnitTests
//
//  Created by 佐藤貴之 on 2023/02/21.
//

import XCTest

final class ParserTests: XCTestCase {
    override func setUpWithError() throws {
    }
    override func tearDownWithError() throws {
    }
    func testDefineStatements() throws {
        let testPatterns: [(input: String, count: Int, expectedIdentifier: String, expectedValues: [Any], isExtended: Bool)] = [
            ("xは、５。", 1, "x", [5], false),
            ("yは、真。", 1, "y", [true], false),
            ("foobarはy。", 1, "foobar", ["y"], false), //「、」が無い定義文の解析
            ("""
                zは、1。※ 改行がある定義文の解析
            
                zは、0。
            """, 2, "z", [1, 0], false),
            ("『割った余り』は、2。",1,"割った余り",[2], false),
            ("正しいは、さらに、関数であって、入力が甲で、本体が、甲である。", 1, "正しい", ["関数であって、【入力が、甲であり、本体が、甲である。】"], true),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            XCTAssertEqual(program.statements.count, test.count)
            for i in 0..<program.statements.count {
                try testDefStatement(
                    program.statements[i],
                    name: test.expectedIdentifier,
                    "は",
                    with: test.expectedValues[i],
                    flag: test.isExtended)
            }
            print("テスト(\(program.string))終了")
        }
    }
    func testReturnExpressions() throws {
        let testPatterns: [(input: String, expectedValue: Any)] = [
            ("5を返す。", 5),
            ("真を返し、", true),
            ("foobarを返す。", "foobar"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            XCTAssertEqual(program.statements.count, 1)
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            try testPhraseExpression(statement.expressions[0], with: test.expectedValue, "を")
            try testKeyword(statement.expressions[1], .RETURN)
            print("テスト終了: \(statement.string)")
        }
    }
    func testIdentifierExpressions() throws {
        let testPatterns: [(input: String, value: String, literal: String)] = [
            ("識別。", "識別", "識別"),
            ("加え、", "加え", "加え"),
            ("二倍し、", "二倍", "二倍"),
            ("『足した数』", "足した数", "足した数"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            XCTAssertEqual(program.statements.count, 1)
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            let ident = try XCTUnwrap(statement.expressions.first as? Identifier)
            XCTAssertEqual(ident.value, test.value)
            XCTAssertEqual(ident.tokenLiteral, test.literal)
            print("テスト終了: \(ident)")
        }
    }
    func testStringLiteralExpression() throws {
        let input = "「みなさん、こんにちは。」"
        print("テストパターン: \(input)")
        let program = try XCTUnwrap(parseProgram(with: input))
        XCTAssertEqual(program.statements.count, 1)
        let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
        let literal = try XCTUnwrap(statement.expressions.first as? StringLiteral)
        XCTAssertEqual(literal.value, "みなさん、こんにちは。")
        XCTAssertEqual(literal.tokenLiteral, "みなさん、こんにちは。")
        print("テスト終了: \(literal)")
    }
    func testIntegerLiteralExpression() throws {
        let input = "-5。"
        print("テストパターン: \(input)")
        let program = try XCTUnwrap(parseProgram(with: input))
        XCTAssertEqual(program.statements.count, 1)
        let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
        let literal = try XCTUnwrap(statement.expressions.first as? IntegerLiteral)
        XCTAssertEqual(literal.value, -5)
        XCTAssertEqual(literal.tokenLiteral, "-5")
        print("テスト終了: \(literal)")
    }
    func testParsingPredicateExpressions() throws {
        let testPatterns: [(input: String, clauses: [(value: Any?, particle: String)], keyword: String)] = [
            ("5と5を足す。", [(5, "と"), (5, "を")], "足す"),
            ("5から5を引く。", [(5, "から"), (5, "を")], "引く"),
            ("5に5を掛ける。", [(5, "に"), (5, "を")], "掛ける"),
            ("5を5で割る。", [(5, "を"), (5, "で")], "割る"),
            ("5が5より大きい。", [(5, "が"), (5, "より")], "大きい"),
            ("5が5より小さい。", [(5, "が"), (5, "より")], "小さい"),
            ("5が5と等しい。", [(5, "が"), (5, "と")], "等しい"),
            ("真と真が等しい。", [(true, "と"), (true, "が")], "等しい"),
            ("真と偽は、等しい。", [(true, "と"), (false, "は")], "等しい"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            XCTAssertEqual(program.statements.count, 1)
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            for (i, clause) in zip(0..<test.clauses.count, test.clauses) {
                let particle = try XCTUnwrap(statement.expressions[i] as? PhraseExpression)
                try testLiteralExpression(particle.left, with: clause.value)
                XCTAssertEqual(particle.tokenLiteral, clause.particle)
            }
            let keyword = try XCTUnwrap(statement.expressions[2] as? PredicateExpression)
            XCTAssertEqual(keyword.string, test.keyword)
            print("テスト終了: \(statement.string)")
        }
    }
    func testParsingNotExpressions() throws {
        let testPatterns: [(input: String, keyword: Token, not: String)] = [
            ("等しくない。", Token(.EQUAL), "ない"),
            ("小さくない。", Token(.LT), "ない"),
            ("大きくない。", Token(.GT), "ない"),
            ("なくない。", Token(.NOT), "ない"),
            ("等しくなく、", Token(.EQUAL), "なく"),
            ("真でない。", Token(.TRUE), "ない"),
            ("偽でなく、", Token(.FALSE), "なく"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            XCTAssertEqual(program.statements.count, 1)
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 2)
            if let particle = statement.expressions[0] as? PhraseExpression { // 助詞「で」
                let boolean = try XCTUnwrap(particle.left as? Boolean)
                XCTAssertEqual(boolean.token.type, test.keyword.type)           // 真偽のチェック
            } else {
                let keyword = try XCTUnwrap(statement.expressions[0] as? PredicateExpression)
                XCTAssertEqual(keyword.token.type, test.keyword.type)           // キーワードのチェック
            }
            let not = try XCTUnwrap(statement.expressions[1] as? PredicateExpression)
            XCTAssertEqual(not.token.type, Token(.NOT).type)                    // 「ない」のチェック
            XCTAssertEqual(not.tokenLiteral, test.not)
            print("テスト終了: \(statement.string)")
        }
    }
    func testBooleanExpressions() throws {
        let testPatterns: [(input: String, exprected: Bool)] = [
            ("真", true), ("偽", false),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            XCTAssertEqual(program.statements.count, 1)
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            let bool = try XCTUnwrap(statement.expressions.first as? Boolean)
            XCTAssertEqual(bool.value, test.exprected)
            print("テスト終了: \(statement.string)")
        }
    }
    func testCaseExpressions() throws {
        let testPatterns = [
            "甲が乙より小さい場合、【甲を表示する】。",
            "甲が乙より小さい場合、【甲を表示する。】。",
            "甲が乙より小さい場合、【甲を表示する。】",
            "甲が乙より小さい場合、【甲を表示する】",
            "甲が乙より小さい場合、甲を表示する。",       // 【】省略形。行末までconsequence
        ]
        for input in testPatterns {
            print("テストパターン: \(input)")
            let program = try XCTUnwrap(parseProgram(with: input))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 4, "statement.expressions.count")
            try testPhraseExpression(statement.expressions[0], with: "甲", "が")
            try testPhraseExpression(statement.expressions[1], with: "乙", "より")
            try testKeywordLiteral(statement.expressions[2], "小さい")
            let caseExpression = try XCTUnwrap(statement.expressions[3] as? CaseExpression) // 場合、
            XCTAssertEqual(caseExpression.consequence.statements.count, 1, "caseExpression.consequence.statements.count")
            let consequence = try XCTUnwrap(caseExpression.consequence.statements.first as? ExpressionStatement)
            XCTAssertEqual(consequence.expressions.count, 3, "consequence.expressions.count")
            try testPhraseExpression(consequence.expressions[0], with: "甲", "を")
            try testKeywordLiteral(consequence.expressions[1], "表示")
            try testKeywordLiteral(consequence.expressions[2], "する")
            XCTAssertNil(caseExpression.alternative, "caseExpression.alternative")
            print("テスト終了: \(statement.string)")
        }
    }
    func testSwitchCaseExpression() throws {
        let input = "3が、1の場合【甲を表示し】、2の場合【乙を表示し】、それ以外は【丙を表示する】。"
        print("テストパターン: \(input)")
        let expecteds = [(1, "甲", "し"), (2, "乙", "し"), (3, "丙", "する"),]
        //
        let program = try XCTUnwrap(parseProgram(with: input))
        XCTAssertEqual(program.statements.count, 1, "program.statements.count")
        let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
        XCTAssertEqual(statement.expressions.count, 3, "statement.expressions.count")
        try testPhraseExpression(statement.expressions[0], with: 3, "が")
        for i in 0...1 {
            let expression = try XCTUnwrap(statement.expressions[i+1] as? GenitiveExpression)
            let value = try XCTUnwrap(expression.left as? IntegerLiteral)
            XCTAssertEqual(value.value, expecteds[i].0)
            let caseExpression = try XCTUnwrap(expression.right as? CaseExpression)
            let consequence = try XCTUnwrap(caseExpression.consequence.statements.first as? ExpressionStatement)
            try testCaseExpressions(consequence.expressions, with: expecteds[i].1, endOfWord: expecteds[i].2)
            if let alternative = caseExpression.alternative?.statements.first as? ExpressionStatement {
                try testCaseExpressions(alternative.expressions, with: expecteds[2].1, endOfWord: expecteds[2].2)
            }
        }
        print("テスト終了: \(statement.string)")
    }
    func testBlockStatementParsings() throws {
        let testPatterns: [(input: String, statements: Int, expressions: Int, consequences: Int)] = [
            ("""
            甲が乙より小さい場合、甲を表示する。
            甲と乙を掛ける。
            """, 2, 4, 1),
            ("""
            甲が乙より小さい場合、【甲を表示する。
            乙から甲を引く。】。甲と乙を掛ける。
            """, 2, 4, 2),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            XCTAssertEqual(program.statements.count, test.statements, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, test.expressions, "statement.expressions.count")
            let caseExpression = try XCTUnwrap(statement.expressions[3] as? CaseExpression)
            XCTAssertEqual(caseExpression.consequence.statements.count, test.consequences, "caseExpression.consequence.consequence.statements.count")
            print("テスト終了: \(statement.string)")
        }
    }
    func testFunctionLiteralWithName() throws {
        let input = "myFunctionは、関数であって、【】。"
        let program = try XCTUnwrap(parseProgram(with: input))
        XCTAssertEqual(program.statements.count, 1)
        let define = try XCTUnwrap(program.statements.first as? DefineStatement)
        let function = try XCTUnwrap(define.value.expressions.first as? FunctionLiteral)
        XCTAssertEqual(function.name, "myFunction")
    }
    func testFunctionLiteralParsings() throws {
        let testPatterns = [
            "関数であって、【入力が、xとyであり、本体が、xにyを足して、返す。】",
            "関数であり、入力が、xとyであり、本体は、xにyを足して、返す。",    //【】省略形。行末まで関数本体
            "関数【入力がxとyであり、xにyを足して、返す。】",                // 省略形
        ]
        for input in testPatterns {
            print("テストパターン: \(input)")
            let program = try XCTUnwrap(parseProgram(with: input))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 1, "statement.expressions.count")
            let function = try XCTUnwrap(statement.expressions.first as? FunctionLiteral)   // 関数であって、
            let functionBlock = function.function
            XCTAssertEqual(functionBlock.parameters.count, 2, "function.parameters.count")
            try testLiteralExpression(functionBlock.parameters[0], with: "x")
            try testLiteralExpression(functionBlock.parameters[1], with: "y")
            XCTAssertEqual(functionBlock.body?.statements.count, 1)
            let bodyStatement = try XCTUnwrap(functionBlock.body?.statements.first as? ExpressionStatement)
            XCTAssertEqual(bodyStatement.expressions.count, 4)
            try testPhraseExpression(bodyStatement.expressions[0], with: "x", "に")
            try testPhraseExpression(bodyStatement.expressions[1], with: "y", "を")
            try testParticleLiteral(bodyStatement.expressions[2], "足して")
            try testKeywordLiteral(bodyStatement.expressions[3],  "返す")
            print("テスト終了: \(statement.string)")
        }
    }
    func testFunctionParameterParsing() throws {
        let testPatterns: [(input: String, expectedParameters: [String])] = [
            ("関数【】", []),
            ("関数であり、【入力がxであり、】", ["x"]),
            ("関数であって、【入力がxとyとzであり、】", ["x","y","z"]),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 1, "statement.expressions.count")
            let function = try XCTUnwrap(statement.expressions.first as? FunctionLiteral)   // 関数であって、
            let functionBlock = function.function
            XCTAssertEqual(functionBlock.parameters.count, test.expectedParameters.count, "関数のパラメータ数が間違っている。")
            try zip(functionBlock.parameters, test.expectedParameters).forEach {
                try testLiteralExpression($0, with: $1)
            }
            print("テスト(\(statement.string))終了")
        }
    }
    func testDefaultParameterParsing() throws {
        let testPatterns: [(input: String, expectedParameters: [String])] = [
            ("関数【入力が、xと、yは1と、zは2。xとyとzを足す】。", ["x","y","z"]),
            ("関数【入力が、x、yは1、zは2。xとyとzを足す】。", ["x","y","z"]),
            ("関数【入力が、xとyは1とzは2。xとyとzを足す】", ["x","y","z"]),
            ("関数【入力がxとyは1とzは2。xとyとzを足す】", ["x","y","z"]),
            ("関数【入力が、xと、yは1と、zは2で、xとyとzを足す】。", ["x","y","z"]),
            ("関数【入力が、xと、yは1と、zは2であり、xとyとzを足す】。", ["x","y","z"]),
            ("関数【入力が、xと、yは1と、zは2】。", ["x","y","z"]),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 1, "statement.expressions.count")
            let function = try XCTUnwrap(statement.expressions.first as? FunctionLiteral)
            let functionBlock = function.function
            XCTAssertEqual(functionBlock.parameters.count, test.expectedParameters.count, "関数のパラメータ数が間違っている。")
            try zip(functionBlock.parameters, test.expectedParameters).forEach {
                try testLiteralExpression($0, with: $1)
            }
            XCTAssertNil(functionBlock.signature.values[0]) // x: 既定値無し
            let valueOfy = try XCTUnwrap(functionBlock.signature.values[1]?.expressions.first as? IntegerLiteral)
            XCTAssertEqual(valueOfy.value, 1)               // y: 既定値1
            let valueOfz = try XCTUnwrap(functionBlock.signature.values[2]?.expressions.first as? IntegerLiteral)
            XCTAssertEqual(valueOfz.value, 2)               // z: 既定値2
            zip(functionBlock.rangeOfInputs, 1...3).forEach {
                XCTAssertEqual($0, $1)                      // パラメータ数: 1〜3
            }
            print("テスト(\(statement.string))終了")
        }
    }
    func testCallExpressionParsing() throws {
        let testPatterns = [
//            "加えるであって、【xは１。yは２】。",
//            "加えるであり、引数が、xは１。yは２。",              //【】省略形
            "加える【引数が、xは１。yは２】。",                 // 省略形
            "加える【xは１。yは２】。",                        // 省略形
        ]
        for input in testPatterns {
            print("テストパターン: \(input)")
            let program = try XCTUnwrap(parseProgram(with: input))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 1, "statement.expressions.count")
            let callExpression = try XCTUnwrap(statement.expressions.first as? CallExpression)
            XCTAssertEqual(callExpression.target.tokenLiteral, "加える")
            let args = callExpression.arguments
            XCTAssertEqual(args.count, 2, "arguments.count")
            XCTAssertEqual(args[0].string, "xは、1。")
            XCTAssertEqual(args[1].string, "yは、2。")
            print("テスト終了: \(statement.string)")
        }
    }
    func testProtocolLiteralParsings() throws {
        let testPatterns = [
            "規約であって、【数値は「数値」。文字列は「文字列」。数値化は関数で、入力が文字列「文字列を」。】",
            "規約であり、数値は「数値」。文字列は「文字列」。数値化は関数で、入力が文字列「文字列を」。",
            "規約【数値は「数値」。文字列は「文字列」。数値化は関数で、入力が文字列「文字列を」】",
            "規約【数値は「数値」。文字列は「文字列」。数値化は関数【入力が文字列「文字列を」】】",
        ]
        for input in testPatterns {
            print("テストパターン: \(input)")
            let program = try XCTUnwrap(parseProgram(with: input))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 1, "statement.expressions.count")
            let protocolLiteral = try XCTUnwrap(statement.expressions.first as? ProtocolLiteral)
            XCTAssertEqual(protocolLiteral.clauses.count, 3, "protocolLiteral.clauses.count")
            XCTAssertEqual(protocolLiteral.clauses[0].string.withoutPeriod, "数値は、「数値」")
            XCTAssertEqual(protocolLiteral.clauses[1].string.withoutPeriod, "文字列は、「文字列」")
            XCTAssertEqual(protocolLiteral.clauses[2].string.withoutPeriod, "数値化は、関数であって、【入力が、文字列「文字列を」】")
            print("テスト終了: \(statement.string)")
        }
    }
    func testTypeLiteralParsings() throws {
        let testPatterns = [
            "型であって、【初期化は、【入力が、xとyであり、xにyを足し「z」に代入する】。本体が、aは１。】",
            "型であり、初期化は、【入力が、xとyで、xにyを足し「z」に代入する】。本体が、aは１。",
            "型【初期化【入力がxとyで、xにyを足し「z」に代入する】。aは１】",
        ]
        for input in testPatterns {
            print("テストパターン: \(input)")
            let program = try XCTUnwrap(parseProgram(with: input))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 1, "statement.expressions.count")
            let typeLiteral = try XCTUnwrap(statement.expressions.first as? TypeLiteral)
            XCTAssertEqual(typeLiteral.initializers.count, 1)
            let params = typeLiteral.initializers.array[0].parameters
            XCTAssertEqual(params.count, 2, "typeLiteral.parameters.count")
            try testLiteralExpression(params[0], with: "x")
            try testLiteralExpression(params[1], with: "y")
            let body = try XCTUnwrap(typeLiteral.initializers.array[0].body)
            XCTAssertEqual(body.statements.count, 1)
            let initStatement = try XCTUnwrap(body.statements.first as? ExpressionStatement)
            XCTAssertEqual(initStatement.expressions.count, 6)
            try testPhraseExpression(initStatement.expressions[0], with: "x", "に")
            try testPhraseExpression(initStatement.expressions[1], with: "y", "を")
            try testKeywordLiteral(initStatement.expressions[2], "足し")
            try testPhraseExpression(initStatement.expressions[3], with: "「z」", "に")
            try testKeywordLiteral(initStatement.expressions[4], "代入")
            try testKeywordLiteral(initStatement.expressions[5], "する")
            XCTAssertNotNil(typeLiteral.body)
            XCTAssertEqual(typeLiteral.body!.statements.count, 1)
            let bodyStatement = try XCTUnwrap(typeLiteral.body!.statements.first as? DefineStatement)
            try testDefStatement(bodyStatement, name: "a", "は", with: 1)
            print("テスト終了: \(statement.string)")
        }
    }
    func testProtocolComformingProtocols() throws {
        let input = "規約であって、準拠する規約は、甲、乙、丙。条項は、丁は、「数値」。"
        print("テストパターン: \(input)")
        let program = try XCTUnwrap(parseProgram(with: input))
        XCTAssertEqual(program.statements.count, 1, "program.statements.count")
        let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
        XCTAssertEqual(statement.expressions.count, 1, "statement.expressions.count")
        let literal = try XCTUnwrap(statement.expressions.first as? ProtocolLiteral)
        XCTAssertEqual(literal.protocols.count, 3)
        XCTAssertEqual(literal.protocols[0], "甲")
        XCTAssertEqual(literal.protocols[1], "乙")
        XCTAssertEqual(literal.protocols[2], "丙")
        XCTAssertEqual(literal.clauses.count, 1)
        XCTAssertEqual(literal.clauses[0].identifier.value, "丁")
        XCTAssertEqual(literal.clauses[0].type, "数値")
        print("テスト終了: \(statement.string)")
    }
    func testEnumLiteralParsings() throws {
        let testPatterns = [
            "列挙であって、【晴は「sunny」と、曇は「cloudy」と、雨は「rainy」と、雪は「snowy」】。",
            "列挙であり、要素が、晴は「sunny」、曇は「cloudy」、雨は「rainy」、雪は「snowy」。",
            "列挙【晴は「sunny」、曇は「cloudy」、雨は「rainy」、雪は「snowy」】",
        ]
        for input in testPatterns {
            print("テストパターン: \(input)")
            let program = try XCTUnwrap(parseProgram(with: input))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 1, "statement.expressions.count")
            let enumLiteral = try XCTUnwrap(statement.expressions.first as? EnumLiteral)
            XCTAssertEqual(enumLiteral.elements.count, 4, "enumLiteral.elements.count")
            XCTAssertEqual(enumLiteral.elements[0].string.withoutPeriod, "晴は、「sunny」")
            XCTAssertEqual(enumLiteral.elements[1].string.withoutPeriod, "曇は、「cloudy」")
            XCTAssertEqual(enumLiteral.elements[2].string.withoutPeriod, "雨は、「rainy」")
            XCTAssertEqual(enumLiteral.elements[3].string.withoutPeriod, "雪は、「snowy」")
            print("テスト終了: \(statement.string)")
        }
    }
    func testEnumeratorLiteralParsings() throws {
        let testPatterns: [(input: String, type: String, name: String)] = [
            ("列挙子は、甲・乙", "甲", "乙"),
            ("列挙子は、・丙", "", "丙"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            let define = try XCTUnwrap(program.statements[0] as? DefineStatement)
            XCTAssertEqual(define.name.value, "列挙子")
            let enumerator = try XCTUnwrap(define.value.expressions[0] as? EnumeratorLiteral)
            XCTAssertEqual(enumerator.type, test.type)
            XCTAssertEqual(enumerator.name, test.name)
            print("テスト終了: \(enumerator.string)")
        }
    }
    func testArrayLiterals() throws {
        let input = "配列であって、【要素が、１と、2に２を掛けたものと、3に３を足したもの】。"
        print("テストパターン: \(input)")
        let program = try XCTUnwrap(parseProgram(with: input))
        let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
        let array = try XCTUnwrap(statement.expressions.first as? ArrayLiteral)
        XCTAssertEqual(array.elements.count, 3)
        try testIntegerLiteral(array.elements[0].expressions[0], value: 1)
        try testPhraseExpression(array.elements[1].expressions[0], with: 2, "に")
        try testPhraseExpression(array.elements[1].expressions[1], with: 2, "を")
        try testParticleLiteral(array.elements[1].expressions[2], "掛けた")
        try testKeywordLiteral(array.elements[1].expressions[3],  "もの")
        try testPhraseExpression(array.elements[2].expressions[0], with: 3, "に")
        try testPhraseExpression(array.elements[2].expressions[1], with: 3, "を")
        try testParticleLiteral(array.elements[2].expressions[2], "足した")
        try testKeywordLiteral(array.elements[2].expressions[3],  "もの")
        print("テスト(\(statement.string))終了")
    }
    func testParsingDictionaryLiteralsStringKeys() throws {
        let input = "辞書【「一」が1、「二」が2、「三」が3】"
        print("テストパターン: \(input)")
        let program = try XCTUnwrap(parseProgram(with: input))
        let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
        let dictionary = try XCTUnwrap(statement.expressions.first as? DictionaryLiteral)
        XCTAssertEqual(dictionary.pairs.count, 3)
        let expected = ["一": 1, "二": 2, "三": 3]
        for expression in dictionary.pairs {
            let literal = try XCTUnwrap(expression.pair.key.expressions.first as? StringLiteral)
            let expectedValue = try XCTUnwrap(expected[literal.value])
            try testIntegerLiteral(expression.pair.value.expressions.first, value: expectedValue)
        }
        print("テスト(\(statement.string))終了")
    }
    func testParsingEmptyDictionaryLiteral() throws {
        let inputs = ["辞書【】", "辞書。"]
        for input in inputs {
            print("テストパターン: \(input)")
            let program = try XCTUnwrap(parseProgram(with: input))
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            let dictionary = try XCTUnwrap(statement.expressions.first as? DictionaryLiteral)
            XCTAssertEqual(dictionary.pairs.count, 0)
            print("テスト(\(statement.string))終了")
        }
   }
    func testParsingDictionaryLiteralWithExpressions() throws {
        let input = "辞書【「一」が0と1を足す、「二」が10から8を引く、「三」が15を5で割る】"
        print("テストパターン: \(input)")
        let program = try XCTUnwrap(parseProgram(with: input))
        let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
        let dictionary = try XCTUnwrap(statement.expressions.first as? DictionaryLiteral)
        XCTAssertEqual(dictionary.pairs.count, 3)
        let testPatterns: [String: ([Expression]) throws -> Void] = [
            "一": { expressions in
                try self.testPhraseExpression(expressions[0], with: 0, "と")
                try self.testPhraseExpression(expressions[1], with: 1, "を")
                try self.testKeyword(expressions[2], .ADD)
            },
            "二": { expressions in
                try self.testPhraseExpression(expressions[0], with: 10, "から")
                try self.testPhraseExpression(expressions[1], with: 8, "を")
                try self.testKeyword(expressions[2], .SUBSTRACT)
            },
            "三": { expressions in
                try self.testPhraseExpression(expressions[0], with: 15, "を")
                try self.testPhraseExpression(expressions[1], with: 5, "で")
                try self.testKeyword(expressions[2], .DIVIDE)
            },
        ]
        for expression in dictionary.pairs {
            let literal = try XCTUnwrap(expression.pair.key.expressions.first as? StringLiteral)
            let testFunction = try XCTUnwrap(testPatterns[literal.value])
            try testFunction(expression.pair.value.expressions)
        }
        print("テスト(\(statement.string))終了")
    }
    func testSelectiveOrExpression() throws {
        let input = "「一」または「二」または「三」または「四」"
        print("テストパターン: \(input)")
        let expected = "「一」、または、「二」、または、「三」、または、「四」。"
        let program = try XCTUnwrap(parseProgram(with: input))
        let statement = try XCTUnwrap(program.statements.first)
        XCTAssertEqual(statement.string, expected)
        print("テスト(\(program.string))終了")
    }
    func testGenitiveExpressions() throws {
        let testPatterns: [(input: String, expected: String)] = [
            ("甲の乙の「一」の１", "甲の乙の「一」の1。"),
            ("甲の乙は、１", "甲の乙は、1。"),
            ("甲の１番目は、１と２を足す", "甲の1は、1と2を足す。"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            let statement = try XCTUnwrap(program.statements.first)
            XCTAssertEqual(statement.string, test.expected)
            print("テスト(\(statement.string))終了")
        }
    }
    func testLogicalExpressions() throws {
        let testPatterns : [(input: String, expected: String)] = [
            ("甲が乙より小さい、または、【甲が１に等しい】", "または"),
            ("甲が乙より小さい、かつ、【甲が１に等しい】", "かつ"),
            ("甲が乙より小さい、または、甲が１に等しい", "または"),
        ]
        
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 4, "statement.expressions.count")
            try testPhraseExpression(statement.expressions[0], with: "甲", "が")
            try testPhraseExpression(statement.expressions[1], with: "乙", "より")
            try testKeywordLiteral(statement.expressions[2], "小さい")
            let logicalExpression = try XCTUnwrap(statement.expressions[3] as? LogicalExpression)
            XCTAssertEqual(logicalExpression.tokenLiteral, test.expected)
            XCTAssertEqual(logicalExpression.right.statements.count, 1, "logicalExpression.right.statements.count")
            let right = try XCTUnwrap(logicalExpression.right.statements.first as? ExpressionStatement)
            XCTAssertEqual(right.expressions.count, 3, "right.expressions.count")
            try testPhraseExpression(right.expressions[0], with: "甲", "が")
            try testPhraseExpression(right.expressions[1], with: 1, "に")
            try testKeywordLiteral(right.expressions[2], "等しい")
            print("テスト終了: \(statement.string)")
        }
    }
    func testLoopExpressions() throws {
        let testPatterns: [(input: String, testFunc: ([Expression]) throws -> Void)] = [
            ("1から1000まで反復【入力が数字で、数字を表示する】。", { expressions in
                try self.testPhraseExpression(expressions[0], with: 1, "から")
                try self.testPhraseExpression(expressions[1], with: 1000, "まで")
                let loop = try XCTUnwrap(expressions[2] as? LoopExpression)
                XCTAssertEqual(loop.token, .keyword(.LOOP))
                try self.testIdentifier(loop.parameters.first, value: "数字")
                let body = try XCTUnwrap(loop.body.statements.first as? ExpressionStatement)
                try self.testPhraseExpression(body.expressions[0], with: "数字", "を")
                try self.testKeywordLiteral(body.expressions[1], "表示")
 //               try self.testKeywordLiteral(body.expressions[2], "する") //　「する」はlexerが無視
            }),
            ("1000から1まで-1ずつ反復【入力が数字で、数字を表示する】。", { expressions in
                try self.testPhraseExpression(expressions[0], with: 1000, "から")
                try self.testPhraseExpression(expressions[1], with: 1, "まで")
                try self.testIntegerLiteral(expressions[2], value: -1)
                let loop = try XCTUnwrap(expressions[3] as? LoopExpression)
                XCTAssertEqual(loop.token, .keyword(.LOOP))
                try self.testIdentifier(loop.parameters.first, value: "数字")
                XCTAssertTrue(loop.condition.isEmpty)
                let body = try XCTUnwrap(loop.body.statements.first as? ExpressionStatement)
                try self.testPhraseExpression(body.expressions[0], with: "数字", "を")
                try self.testKeywordLiteral(body.expressions[1], "表示")
//                try self.testKeywordLiteral(body.expressions[2], "する") //　「する」はlexerが無視
            }),
            ("配列【１、２、３】を反復【入力が数字で、数字を表示する】。", { expressions in
                let phrase = try XCTUnwrap(expressions[0] as? PhraseExpression)
                let array = try XCTUnwrap(phrase.left as? ArrayLiteral)
                XCTAssertEqual(array.elements.count, 3)
                let loop = try XCTUnwrap(expressions[1] as? LoopExpression)
                XCTAssertEqual(loop.token, .keyword(.LOOP))
                try self.testIdentifier(loop.parameters.first, value: "数字")
                XCTAssertTrue(loop.condition.isEmpty)
                let body = try XCTUnwrap(loop.body.statements.first as? ExpressionStatement)
                try self.testPhraseExpression(body.expressions[0], with: "数字", "を")
                try self.testKeywordLiteral(body.expressions[1], "表示")
//                try self.testKeywordLiteral(body.expressions[2], "する")    //　「する」はlexerが無視
            }),
            ("１から反復【1を足し、100より大きい場合、【中止する】】。", { expressions in
                try self.testPhraseExpression(expressions[0], with: 1, "から")
                let loop = try XCTUnwrap(expressions[1] as? LoopExpression)
                XCTAssertEqual(loop.token, .keyword(.LOOP))
                XCTAssertTrue(loop.parameters.isEmpty)
                XCTAssertTrue(loop.condition.isEmpty)
                let body = try XCTUnwrap(loop.body.statements.first as? ExpressionStatement)
                try self.testPhraseExpression(body.expressions[0], with: 1, "を")
                try self.testKeywordLiteral(body.expressions[1], "足し")
                try self.testPhraseExpression(body.expressions[2], with: 100, "より")
                try self.testKeywordLiteral(body.expressions[3], "大きい")
                let caseExpression = try XCTUnwrap(body.expressions[4] as? CaseExpression)
                let consequece = try XCTUnwrap(caseExpression.consequence.statements.first as? ExpressionStatement)
                try self.testKeywordLiteral(consequece.expressions[0], "中止")
//                try self.testKeywordLiteral(consequece.expressions[1], "する")  //　「する」はlexerが無視
            }),
            ("１から反復【条件が、100より小さい間、1を足す】。", { expressions in
                try self.testPhraseExpression(expressions[0], with: 1, "から")
                let loop = try XCTUnwrap(expressions[1] as? LoopExpression)
                XCTAssertEqual(loop.token, .keyword(.LOOP))
                XCTAssertTrue(loop.parameters.isEmpty)
                let expressions = loop.condition
                try self.testPhraseExpression(expressions[0], with: 100, "より")
                try self.testKeywordLiteral(expressions[1], "小さい")
                let body = try XCTUnwrap(loop.body.statements.first as? ExpressionStatement)
                try self.testPhraseExpression(body.expressions[0], with: 1, "を")
                try self.testKeywordLiteral(body.expressions[1], "足す")
            }),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            try test.testFunc(statement.expressions)
            print("テスト終了: \(statement.string)")
        }
    }
    func testRangeLiterals() throws {
        let testPatterns: [(input: String, expecteds: [Token?])] = [
            ("範囲【1以上】",     [Token(.GTEQUAL), nil]),
            ("範囲【10以下】",    [nil, Token(.LTEQUAL)]),
            ("範囲【10未満】",    [nil, Token(.UNDER)]),
            ("範囲であって、1以上。",     [Token(.GTEQUAL), nil]),
            ("範囲【1から10まで】", [Token(.KARA), Token(.MADE)]),
            ("範囲【1以上10以下】", [Token(.GTEQUAL), Token(.LTEQUAL)]),
            ("範囲【1以上10未満】", [Token(.GTEQUAL), Token(.UNDER)]),
            ("範囲であって、1から10まで。", [Token(.KARA), Token(.MADE)]),
            ("1以上",             [Token(.GTEQUAL), nil]),
            ("10以下",            [nil, Token(.LTEQUAL)]),
            ("10未満",            [nil, Token(.UNDER)]),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            let range = try XCTUnwrap(statement.expressions.first as? RangeLiteral)
            if let lowerBound = range.lowerBound {
                let integer = try XCTUnwrap(lowerBound.expressions.first as? IntegerLiteral)
                XCTAssertEqual(integer.value, 1)
                XCTAssertEqual(lowerBound.token, test.expecteds[0])
            }
            if let upperBound = range.upperBound {
                let integer = try XCTUnwrap(upperBound.expressions.first as? IntegerLiteral)
                XCTAssertEqual(integer.value, 10)
                XCTAssertEqual(upperBound.token, test.expecteds[1])
            }
            print("テスト終了: \(statement.string)")
        }
    }
    func testRangeExpressions() throws {
        let testPatterns: [(input: String, testFunc: (ExpressionStatement?, ExpressionStatement?) throws -> Void)] = [
            ("範囲【1に1を足すから、100で10を割るまで】", { lowerBound, upperBound in
                if lowerBound != nil {
                    let expressions = lowerBound!.expressions
                    XCTAssertEqual(lowerBound!.token, Token(.KARA))
                    try self.testPhraseExpression(expressions[0], with: 1, "に")
                    try self.testPhraseExpression(expressions[1], with: 1, "を")
                    try self.testKeywordLiteral(expressions[2], "足す")
                }
                if upperBound != nil {
                    let expressions = upperBound!.expressions
                    XCTAssertEqual(upperBound!.token, Token(.MADE))
                    try self.testPhraseExpression(expressions[0], with: 100, "で")
                    try self.testPhraseExpression(expressions[1], with: 10, "を")
                    try self.testKeywordLiteral(expressions[2], "割る")
                }
            }),
            ("範囲【1に1を足す以上、100で10を割る未満】", { lowerBound, upperBound in
                if lowerBound != nil {
                    let expressions = lowerBound!.expressions
                    XCTAssertEqual(lowerBound!.token, Token(.GTEQUAL))
                    try self.testPhraseExpression(expressions[0], with: 1, "に")
                    try self.testPhraseExpression(expressions[1], with: 1, "を")
                    try self.testKeywordLiteral(expressions[2], "足す")
                }
                if upperBound != nil {
                    let expressions = upperBound!.expressions
                    XCTAssertEqual(upperBound!.token, Token(.UNDER))
                    try self.testPhraseExpression(expressions[0], with: 100, "で")
                    try self.testPhraseExpression(expressions[1], with: 10, "を")
                    try self.testKeywordLiteral(expressions[2], "割る")
                }
            }),
            ("範囲【甲以上乙以下】", { lowerBound, upperBound in
                if lowerBound != nil {
                    let expressions = lowerBound!.expressions
                    XCTAssertEqual(lowerBound!.token, Token(.GTEQUAL))
                    try self.testIdentifier(expressions[0], value: "甲")
                }
                if upperBound != nil {
                    let expressions = upperBound!.expressions
                    XCTAssertEqual(upperBound!.token, Token(.LTEQUAL))
                    try self.testIdentifier(expressions[0], value: "乙")
                }
            }),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            let range = try XCTUnwrap(statement.expressions.first as? RangeLiteral)
            try test.testFunc(range.lowerBound, range.upperBound)
            print("テスト終了: \(statement.string)")
        }
    }
    func testFunctionSignatures() throws {
        let testPatterns: [(input: String, number: Int?, type: String, particle: String, threeDots: String)] = [
            ("関数【入力が、甲。甲を表示する】", 1, "", "", ""),
            ("関数【入力が、甲「の」。甲を表示する】", 1, "", "の", ""),
            ("関数【入力が、甲「文字列」。甲を表示する】", 1, "文字列", "", ""),
            ("関数【入力が、甲「配列を」。甲を表示する】", 1, "配列", "を", ""),
            ("関数【入力が、甲と乙と丙。甲を表示する】", 3, "", "", ""),
            ("関数【入力が、甲「…」。甲を表示する】", nil, "", "", "…"),
            ("関数【入力が、甲「と…」。甲を表示する】", nil, "", "と", "…"),
            ("関数【入力が、甲「数値と…」。甲を表示する】", nil, "数値", "と", "…"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            let function = try XCTUnwrap(statement.expressions.first as? FunctionLiteral)
            let functionBlock = function.function
            XCTAssertEqual(functionBlock.signature.numberOfInputs, test.number)
            XCTAssertEqual(functionBlock.signature.formats.first?.type, test.type)
            XCTAssertEqual(functionBlock.signature.formats.first?.particle, test.particle)
            XCTAssertEqual(functionBlock.signature.formats.first?.threeDots, test.threeDots)
            print("テスト終了: \(statement.string)")
        }
    }
    func testLabelExpressions() throws {
        let testPatterns: [(input: String, label: String, name: String)] = [
            ("識別子「二倍」", "識別子", "二倍"),
            ("識別子『割った余り』", "識別子", "割った余り"),
            ("ファイル「サンプル」", "ファイル", "サンプル"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            let label = try XCTUnwrap(statement.expressions.first as? Label)
            XCTAssertEqual(label.tokenLiteral, test.label)
            XCTAssertEqual(label.value.literal, test.name)
            print("テスト終了: \(statement.string)")
        }
    }
    func testEndOfStatements() throws {
        let testPatterns: [(input: String, statements: Int, expressions: Int)] = [
            // コメント内番号は、想定する句点、読点、】、EOL、EOFの組み合わせパターン
            ("甲は１。乙は２。",2,1),   // 1. EOF, 2. 。次, 3. 。EOF
            ("関数【甲を表示する。】",1,1),    // 4. 。】, 11. 】EOF
            ("関数【甲を表示する。乙を表示する。】",1,1), // 2. 。次
            ("""
                甲を表示する。
                乙を表示する。
            """,2,3),   // 5. 。EOL, 12. EOL次
            ("関数【甲を表示する】を表示する。",1,3),    // 6. 】次
            ("甲が1より小さい場合【甲を表示する。】乙を表示する。",2,4), // 6. 】次
            ("甲が1より小さい場合【甲を表示し】、それ以外は【乙を表示する】。",1,4),    // 7. 】、9. 】。
            ("関数【甲が1より小さい場合、【甲を表示する】】",1,1),    // 8. 。】
            ("""
                甲が1より小さい場合【甲を表示する。】
                乙を表示する。
            """,2,4), // 10. 】EOL
            ("""
                関数【甲を表示する。
                    乙を表示する。】
            """,1,1), // 12. EOL次
            ("""
                甲は１。
            
            """,1,1),   // 13. EOL EOF
            ("""
                関数【
                    甲が1より小さい場合、【甲を表示する】
                】
            """,1,1),    // 14. EOL 】
            ("""
            
            
                甲は１。
            
            
            """,1,1),   // 15. EOL EOL
            ("""
                関数【

                    甲を表示する。
            
                】
            """,1,1),   // 15. EOL EOL
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            XCTAssertEqual(program.statements.count, test.statements, "program.statements.count")
            if let statement = program.statements.first as? DefineStatement {
                XCTAssertEqual(statement.value.expressions.count, test.expressions, "statement.expressions.count")
            } else
            if let statement = program.statements.first as? ExpressionStatement {
                XCTAssertEqual(statement.expressions.count, test.expressions, "statement.expressions.count")
            }
            print("テスト終了: \(program.string)")
        }

    }
    // MARK: - ヘルパー
    private func testDefStatement(_ statement: Statement?, name: String, _ string: String, with value: Any, flag: Bool = false) throws {
        let defStatement = try XCTUnwrap(statement as? DefineStatement)
        XCTAssertEqual(defStatement.token.literal, string)
        XCTAssertEqual(defStatement.name.value, name)
        XCTAssertEqual(defStatement.name.tokenLiteral, name)
        try testLiteralExpression(defStatement.value.expressions.first!, with: value)
        XCTAssertEqual(defStatement.isExtended, flag)
    }
    private func testPhraseExpression(_ expression: Expression,  with expected: Any, _ particle: String) throws {
        let p = try XCTUnwrap(expression as? PhraseExpression)
        XCTAssertEqual(p.tokenLiteral, particle)
        try testLiteralExpression(p.left, with: expected)
    }
    private func testParticleLiteral(_ expression: Expression, _ particle: String) throws {
        let p = try XCTUnwrap(expression as? PhraseExpression)
        XCTAssertEqual(p.string, particle)
    }
    private func testKeywordLiteral(_ expression: Expression, _ keyword: String) throws {
        let k = try XCTUnwrap(expression as? PredicateExpression)
        XCTAssertEqual(k.string, keyword)
    }
    private func testKeyword(_ expression: Expression, _ keyword: Token.Keyword) throws {
        let k = try XCTUnwrap(expression as? PredicateExpression)
        let type = Token.TokenType.keyword(keyword)
        XCTAssertEqual(k.token.type, type)
    }
    private func testLiteralExpression(_ expression: Expression?, with expected: Any?) throws {
        switch expected {
        case let int as Int:
            try testIntegerLiteral(expression, value: int)
        case let string as String:
            XCTAssertEqual(expression?.string, string)
        case let bool as Bool:
            try testBooleanLiteral(expression, value: bool)
        case let token as Token:
            try testKeywordLiteral(expression!, token.literal)
        default:
            XCTFail("テスト値の型(\(type(of: expected)))は未サポート。")
        }
    }
    private func testIdentifier(_ expression: Expression?, value: String) throws {
        let identifier = try XCTUnwrap(expression as? Identifier)
        XCTAssertEqual(identifier.value, value)
        XCTAssertEqual(identifier.tokenLiteral, value)
    }
    private func testIntegerLiteral(_ expression: Expression?, value: Int) throws {
        let integerLiteral = try XCTUnwrap(expression as? IntegerLiteral)
        XCTAssertEqual(integerLiteral.value, value)
        XCTAssertEqual(integerLiteral.tokenLiteral, String(value))
    }
    private func testBooleanLiteral(_ expression: Expression?, value: Bool) throws {
        let boolean = try XCTUnwrap(expression as? Boolean)
        XCTAssertEqual(boolean.value, value)
        XCTAssertEqual(boolean.tokenLiteral, value ? "真" : "偽")
    }
    private func testCaseExpressions(_ expressions: [Expression], with ident: String, endOfWord: String) throws {
        XCTAssertEqual(expressions.count, 3, "expressions.count")
        try testPhraseExpression(expressions[0], with: ident, "を")
        try testKeywordLiteral(expressions[1], "表示")
        try testKeywordLiteral(expressions[2], endOfWord)
    }
}
