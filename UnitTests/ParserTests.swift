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
            ("正しいは、さらに、関数であって、入力が甲、本体が、甲である。", 1, "正しい", ["関数であって、【入力が、甲であり、本体が、甲である。】"], true),
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
        let expecteds = [(1, "甲"), (2, "乙"), (3, "丙"),]
        let program = try XCTUnwrap(parseProgram(with: input))
        XCTAssertEqual(program.statements.count, 1, "program.statements.count")
        let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
        XCTAssertEqual(statement.expressions.count, 5, "statement.expressions.count")
        try testPhraseExpression(statement.expressions[0], with: 3, "が")
        var alternative: ExpressionStatement?
        for (i, expected) in zip([1, 3], [expecteds[0], expecteds[1]]) {
            try testPhraseExpression(statement.expressions[i], with: expected.0, "の")
            let caseExpression = try XCTUnwrap(statement.expressions[i+1] as? CaseExpression) // 場合、
            XCTAssertEqual(caseExpression.consequence.statements.count, 1, "caseExpression.consequence.statements.count")
            let consequence = try XCTUnwrap(caseExpression.consequence.statements.first as? ExpressionStatement)
            XCTAssertEqual(consequence.expressions.count, 3, "consequence.expressions.count")
            try testPhraseExpression(consequence.expressions[0], with: expected.1, "を")
            try testKeywordLiteral(consequence.expressions[1], "表示")
            try testKeywordLiteral(consequence.expressions[2], "し")
            alternative = caseExpression.alternative?.statements.first as? ExpressionStatement
        }
        XCTAssertEqual(alternative?.expressions.count, 3, "alternative.expressions.count")
        try testPhraseExpression(alternative!.expressions[0], with: expecteds[2].1, "を")
        try testKeywordLiteral(alternative!.expressions[1], "表示")
        try testKeywordLiteral(alternative!.expressions[2], "する")
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
            XCTAssertEqual(function.parameters.count, 2, "function.parameters.count")
            try testLiteralExpression(function.parameters[0], with: "x")
            try testLiteralExpression(function.parameters[1], with: "y")
            XCTAssertEqual(function.body.statements.count, 1)
            let bodyStatement = try XCTUnwrap(function.body.statements.first as? ExpressionStatement)
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
            XCTAssertEqual(function.parameters.count, test.expectedParameters.count, "関数のパラメータ数が間違っている。")
            try zip(function.parameters, test.expectedParameters).forEach {
                try testLiteralExpression($0, with: $1)
            }
            print("テスト(\(statement.string))終了")
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
        let input = "辞書【】"
        print("テストパターン: \(input)")
        let program = try XCTUnwrap(parseProgram(with: input))
        let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
        let dictionary = try XCTUnwrap(statement.expressions.first as? DictionaryLiteral)
        XCTAssertEqual(dictionary.pairs.count, 0)
        print("テスト(\(statement.string))終了")
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
        let expecteds = ["一", "二", "三", "四"]
        let program = try XCTUnwrap(parseProgram(with: input))
        let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
        var orExpression = try XCTUnwrap(statement.expressions.first as? InfixExpression)
        var object = try XCTUnwrap(orExpression.left as? StringLiteral)
        for expected in expecteds {
            XCTAssertEqual(object.value, expected)
            XCTAssertEqual(orExpression.token, .keyword(.OR))
            if let expression = orExpression.right as? InfixExpression {
                orExpression = expression
                object = try XCTUnwrap(orExpression.left as? StringLiteral)
            } else {
                object = try XCTUnwrap(orExpression.right as? StringLiteral)
            }
        }
        print("テスト(\(statement.string))終了")
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
            ("1から1000まで反復【入力が数字、数字を表示する】。", { expressions in
                try self.testPhraseExpression(expressions[0], with: 1, "から")
                try self.testPhraseExpression(expressions[1], with: 1000, "まで")
                let loop = try XCTUnwrap(expressions[2] as? LoopExpression)
                XCTAssertEqual(loop.token, .keyword(.LOOP))
                try self.testIdentifier(loop.parameters.first, value: "数字")
                let body = try XCTUnwrap(loop.body.statements.first as? ExpressionStatement)
                try self.testPhraseExpression(body.expressions[0], with: "数字", "を")
                try self.testKeywordLiteral(body.expressions[1], "表示")
                try self.testKeywordLiteral(body.expressions[2], "する")
            }),
            ("1000から1まで-1ずつ反復【入力が数字、数字を表示する】。", { expressions in
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
                try self.testKeywordLiteral(body.expressions[2], "する")
            }),
            ("配列【１、２、３】を反復【入力が数字、数字を表示する】。", { expressions in
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
                try self.testKeywordLiteral(body.expressions[2], "する")
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
                try self.testKeywordLiteral(consequece.expressions[1], "する")
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
            ("範囲【1から10まで】", [Token(.KARA), Token(.MADE)]),
            ("範囲【1以上10以下】", [Token(.GTEQUAL), Token(.LTEQUAL)]),
            ("範囲【1以上10未満】", [Token(.GTEQUAL), Token(.UNDER)]),
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
        let testPatterns: [(input: String, number: Int?, type: String, particle: String)] = [
            ("関数【入力が、甲、甲を表示する】", 1, "", ""),
            ("関数【入力が、甲「の」、甲を表示する】", 1, "", "の"),
            ("関数【入力が、甲「文字列」、甲を表示する】", 1, "文字列", ""),
            ("関数【入力が、甲「配列を」、甲を表示する】", 1, "配列", "を"),
            ("関数【入力が、甲と乙と丙、甲を表示する】", 3, "", ""),
            ("関数【入力が、甲「と…」、甲を表示する】", nil, "", "と…"),
            ("関数【入力が、甲「数値と…」、甲を表示する】", nil, "数値", "と…"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input))
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            let function = try XCTUnwrap(statement.expressions.first as? FunctionLiteral)
            XCTAssertEqual(function.signature.numberOfInputs, test.number)
            XCTAssertEqual(function.signature.formats.first?.type, test.type)
            XCTAssertEqual(function.signature.formats.first?.particle, test.particle)
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
            XCTAssertEqual(label.value, test.name)
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
    private func parseProgram(with input: String) -> Program? {
        let lexer = Lexer(input)
 /*       print("tagged:\t\t", terminator: "")
        lexer.enumerated.forEach {print($0 + "(\($1))", terminator: "")}
        print() */
        let parser = Parser(lexer)
        let program = parser.parseProgram()
        check(parser.errors)
        return program
    }
    private func check(_ errors: [String]) {
        if errors.isEmpty {return}
        print("Parserが、\(errors.count)個のエラーを検出した。")
        errors.forEach {print("Parerエラー: \($0)")}
        XCTFail()
    }
    //
    private func testDefStatement(_ statement: Statement?, name: String, _ string: String, with value: Any, flag: Bool) throws {
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
}
