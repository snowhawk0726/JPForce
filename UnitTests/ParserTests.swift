//
//  ParserTests.swift
//  UnitTests
//
//  Created by 佐藤貴之 on 2023/02/21.
//

import XCTest

final class ParserTests: XCTestCase {
    var useSentenceAST = true
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
            ("正しいは、さらに、関数であって、入力が甲で、本体が、甲である。", 1, "正しい", ["関数であって、【入力が、甲であり、本体が、甲である】"], true),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, test.count)
            for i in 0..<program.statements.count {
                try testDefineStatement(
                    program.statements[i],
                    target: test.expectedIdentifier,
                    with: test.expectedValues[i],
                    flag: test.isExtended)
            }
            print("テスト終了：\(program.string) (rhs = \(valueType(in: program)))")
        }
    }
    private func valueType(in program: Program) -> String {
        let rhsLiterals = program.statements.compactMap { ($0 as? DefineStatement)?.rhsLiteral }
        guard rhsLiterals.count == program.statements.count else { return "?" }
        let types = rhsLiterals.map { String(describing: type(of: $0)) }
        return types.joined(separator: ",")
    }
    func testRhsStatement() throws {
        let testPatterns: [(input: String, type: String?)] = [
            ("aは、1。", "ExpressionStatement"),
            ("aは、1と2を足す。", "SimpleSentence"),
            ("aは、1と2を足したもの。", "ExpressionStatement"),
            ("aは、1と2を足し、3で割る", "CompoundStatement"),
            ("aは、1と2を足し、bに代入", nil),
            ("aは、1と2を足し、bに代入したもの。", "CompoundStatement"), // TODO: - ModifiedNounExpression導入時に、エラーにする
            ("aのbは、1。", "ExpressionStatement"),
            ("aのbは、1と2を足す。", "SimpleSentence"),
            ("aのbは、1と2を足したもの。", "ExpressionStatement"),
            ("aのbは、1と2を足し、3で割る", "CompoundStatement"),
            ("aのbは、cに代入する。", nil),
            ("aのbは、1と2を足し、bに代入したもの。", "CompoundStatement"), // TODO: - ModifiedNounExpression導入時に、エラーにする
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            guard let program = parseProgram(with: test.input, useSentenceAST: true) else {
                XCTAssertNil(test.type)
                print("テスト終了：rhs = エラー")
                continue
            }
            var rhs: Statement?
            if let define = program.statements.first as? DefineStatement {
                rhs = define.value
            } else
            if let es = program.statements.first as? ExpressionStatement,
               let genitive = es.expressions.first as? GenitiveExpression {
                rhs = genitive.value
            } else {
                XCTFail("右辺が存在しない型：\(String(describing: type(of: program.statements.first!)))")
            }
            let typeString = String(describing: type(of: try XCTUnwrap(rhs)))
            XCTAssertEqual(typeString, test.type!)
            print("テスト終了：rhs = \(rhs?.string ?? "<nil>")(\(typeString))")
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
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, 1)
            let statement = try XCTUnwrap(program.statements.first)
            try testPhraseExpression(statement.arguments[0], with: test.expectedValue, "を")
            try testKeyword(statement.predicate, .RETURN)
            print("テスト終了: \(statement.string)(\(type(of: statement)))")
        }
    }
    func testIdentifierExpressions() throws {
        let testPatterns: [(input: String, value: String, literal: String)] = [
            // 入力, 識別子名, リテラル
            ("識別。", "識別", "識別"),
            ("加え、", "加え", "加え"),
            ("加える。", "加える", "加える"),
            ("二倍し、", "二倍", "二倍"),
            ("『足した数』", "足した数", "足した数"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, 1)
            let statement = try XCTUnwrap(program.statements.first)
            let ident = try XCTUnwrap(statement.literal as? Identifier)
            XCTAssertEqual(ident.value, test.value)
            XCTAssertEqual(ident.tokenLiteral, test.literal)
            print("テスト終了: \(ident.value) (in \(type(of: statement)))")
        }
    }
    func testStringLiteralExpression() throws {
        let input = "「みなさん、こんにちは。」"
        print("テストパターン: \(input)")
        let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
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
        let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
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
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, 1)
            let statement = try XCTUnwrap(program.statements.first)
            for (i, clause) in test.clauses.enumerated() {
                let phrase = try XCTUnwrap(statement.arguments[i] as? PhraseExpression)
                try testLiteralExpression(phrase.left, with: clause.value)
                XCTAssertEqual(phrase.tokenLiteral, clause.particle)
            }
            let predicate = try XCTUnwrap(statement.predicate)
            try testKeywordLiteral(predicate, test.keyword)
            print("テスト終了: \(statement.string)(\(type(of: statement)))")
        }
    }
    func testParsingNotExpressions() throws {
        let testPatterns: [(input: String, keyword: Token.Keyword, literal: String)] = [
            ("等しくない。", .EQUAL, "ない"),
            ("小さくない。", .LT, "ない"),
            ("大きくない。", .GT, "ない"),
            ("なくない。", .NOT, "ない"),
            ("等しくなく、", .EQUAL, "なく"),
            ("真でない。", .TRUE, "ない"),
            ("偽でなく、", .FALSE, "なく"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, 1)
            if useSentenceAST {
                if let cs = program.statements.first as? CompoundStatement {
                    let predicate = try XCTUnwrap((cs.sentences.first as? SimpleSentence)?.predicate)
                    try testKeyword(predicate, test.keyword)
                    let literal = try XCTUnwrap((cs.sentences.last as? SimpleSentence)?.predicate?.tokenLiteral)
                    XCTAssertEqual(literal, test.literal)
                } else
                if let ss = program.statements.first as? SimpleSentence {
                    let phrase = try XCTUnwrap(ss.arguments[0] as? PhraseExpression)
                    let boolean = try XCTUnwrap(phrase.left as? Boolean)
                    XCTAssertEqual(boolean.token.keyword, test.keyword)
                    let literal = try XCTUnwrap(ss.predicate?.tokenLiteral)
                    XCTAssertEqual(literal, test.literal)
                } else {
                    XCTFail()
                }
            } else {
                let es = try XCTUnwrap(program.statements.first as? ExpressionStatement)
                XCTAssertEqual(es.expressions.count, 2)
                if let predicate = es.expressions[0] as? PredicateExpression {
                    try testKeyword(predicate, test.keyword)
                } else
                if let phrase = es.arguments[0] as? PhraseExpression {
                    let boolean = try XCTUnwrap(phrase.left as? Boolean)
                    XCTAssertEqual(boolean.token.keyword, test.keyword) // 真偽値のチェック
                } else {
                    XCTFail()
                }
                let negation = try XCTUnwrap(es.expressions[1] as? PredicateExpression)
                XCTAssertEqual(negation.tokenLiteral, test.literal)
            }
            print("テスト終了: \(program.string)(\(type(of: program.statements.first!)))")
        }
    }
    func testBooleanExpressions() throws {
        let testPatterns: [(input: String, exprected: Bool)] = [
            ("真", true), ("偽", false),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, 1)
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            try testBooleanLiteral(statement.literal, value: test.exprected)
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
            let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            if useSentenceAST {
                let statement = try XCTUnwrap(program.statements.first as? CompoundStatement)
                XCTAssertEqual(statement.sentences.count, 2, "statement.sentences.count")
                try testSimpleSentence(statement.sentences[0], predicate: "小さい", phrases: "甲が", "乙より")
                let caseExpression = try XCTUnwrap(statement.sentences[1].literal as? CaseExpression) // 場合、
                let sentence = try XCTUnwrap(caseExpression.consequence.statements.first as? SimpleSentence)
                try testSimpleSentence(sentence, predicate: "表示する", phrases: "甲を")
                XCTAssertNil(caseExpression.alternative, "caseExpression.alternative")
                print("テスト終了: \(statement.string)(\(type(of: statement)))")
            } else {
                let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
                XCTAssertEqual(statement.expressions.count, 4, "statement.expressions.count")
                try testPhraseExpression(statement.expressions[0], with: "甲が")
                try testPhraseExpression(statement.expressions[1], with: "乙より")
                try testKeywordLiteral(statement.expressions[2], "小さい")
                let caseExpression = try XCTUnwrap(statement.expressions[3] as? CaseExpression) // 場合、
                XCTAssertEqual(caseExpression.consequence.statements.count, 1, "caseExpression.consequence.statements.count")
                let consequence = try XCTUnwrap(caseExpression.consequence.statements.first as? ExpressionStatement)
                XCTAssertEqual(consequence.expressions.count, 2, "consequence.expressions.count")
                try testPhraseExpression(consequence.expressions[0], with: "甲を")
                try testKeywordLiteral(consequence.expressions[1], "表示する")
                XCTAssertNil(caseExpression.alternative, "caseExpression.alternative")
                print("テスト終了: \(statement.string)(\(type(of: statement)))")
            }
        }
    }
    func testSwitchCaseExpression() throws {
        let input = "3が、1の場合【甲を表示し】、2の場合【乙を表示し】、それ以外は【丙を表示する】。"
        print("テストパターン: \(input)")
        let expected: [(value: Int?, string: String)] = [
            (1, "甲を表示し"), (2, "乙を表示し"), (nil, "丙を表示する"),
        ]
        //
        let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
        XCTAssertEqual(program.statements.count, 1, "program.statements.count")
        if useSentenceAST {
            let cs = try XCTUnwrap(program.statements.first as? CompoundStatement)
            let es1 = try XCTUnwrap(cs.sentences[0] as? ExpressionStatement)
            let es2 = try XCTUnwrap(cs.sentences[1] as? ExpressionStatement)
            try testPhraseExpression(es1.expressions[0], with: "3が")
            for i in 0..<expected.count {
                let pattern = expected[i].value.map {String($0)}
                let body = expected[i].string
                if i == 0 {
                    try testGenitiveCase(es1.expressions[1], lhs: pattern, rhs: body)
                    continue
                }
                try testGenitiveCase(es2.expressions[0], lhs: pattern, rhs: body)
            }
            print("テスト終了: \(cs.string)(\(type(of: cs)))")
            return
        }
        let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
        XCTAssertEqual(statement.expressions.count, 3, "statement.expressions.count")
        try testPhraseExpression(statement.expressions[0], with: "3が")
        for i in 0..<expected.count {
            let pattern = expected[i].value.map {String($0)}
            let body = expected[i].string
            if i == 0 {
                try testGenitiveCase(statement.expressions[1], lhs: pattern, rhs: body)
                continue
            }
            try testGenitiveCase(statement.expressions[2], lhs: pattern, rhs: body)
        }
        print("テスト終了: \(statement.string)(\(type(of: statement)))")
    }
    private func testGenitiveCase(_ exp: Expression?, lhs: String?, rhs: String, isConsequence: Bool = true) throws {
        let ge = try XCTUnwrap(exp as? GenitiveExpression)
        if let lhs {
            XCTAssertEqual(ge.left.string, lhs)
            try testCaseConsequence(ge.right, with: rhs)
        } else {
            try testCaseAlternative(ge.right, with: rhs)
        }
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
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, test.statements, "program.statements.count")
            if useSentenceAST {
                let cs = try XCTUnwrap(program.statements.first as? CompoundStatement)
                let es = try XCTUnwrap(cs.sentences[1] as? ExpressionStatement)
                let caseExpression = try XCTUnwrap(es.expressions[0] as? CaseExpression)
                XCTAssertEqual(caseExpression.consequence.statements.count, test.consequences, "caseExpression.consequence.statements.count")
                print("テスト終了: \(cs.string)(\(type(of: cs)))")
                continue
            }
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, test.expressions, "statement.expressions.count")
            let caseExpression = try XCTUnwrap(statement.expressions[3] as? CaseExpression)
            XCTAssertEqual(caseExpression.consequence.statements.count, test.consequences, "caseExpression.consequence.statements.count")
            print("テスト終了: \(statement.string)(\(type(of: statement)))")
        }
    }
    func testFunctionLiteralWithName() throws {
        let input = "myFunctionは、関数であって、【】。"
        let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
        XCTAssertEqual(program.statements.count, 1)
        let define = try XCTUnwrap(program.statements.first as? DefineStatement)
        let function = try XCTUnwrap(define.rhsLiteral as? FunctionLiteral)
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
            let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first)
            XCTAssertEqual(statement.expressionCount, 1, "statement.expressions.count")
            let function = try XCTUnwrap(statement.literal as? FunctionLiteral)   // 関数であって、
            let functionBlock = function.function
            XCTAssertEqual(functionBlock.parameters.count, 2, "function.parameters.count")
            try testLiteralExpression(functionBlock.parameters[0], with: "x")
            try testLiteralExpression(functionBlock.parameters[1], with: "y")
            XCTAssertEqual(functionBlock.body?.statements.count, 1)
            if useSentenceAST {
                let body = try XCTUnwrap(functionBlock.body?.statements.first as? CompoundStatement)
                try testPhraseExpression(body.sentences[0].arguments[0], with: "xに")
                try testPhraseExpression(body.sentences[0].arguments[1], with: "yを")
                try testKeywordLiteral(body.sentences[0].predicate, "足し")
                try testKeywordLiteral(body.sentences[1].predicate, "返す")
                print("テスト終了: \(statement.string)")
                continue
            }
            let body = try XCTUnwrap(functionBlock.body?.statements.first as? ExpressionStatement)
            try testPhraseExpression(body.expressions[0], with: "x", "に")
            try testPhraseExpression(body.expressions[1], with: "y", "を")
            try testParticleLiteral(body.expressions[2], "足して")
            try testKeywordLiteral(body.expressions[3],  "返す")
            print("テスト終了: \(statement.string)")
        }
    }
    func testFunctionParameterParsing() throws {
        let testPatterns: [(input: String, expectedParameters: [String])] = [
            ("関数【】", []),
            ("関数であり、【入力がx】", ["x"]),
            ("関数であって、【入力がxとyとz】", ["x","y","z"]),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
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
    func testFunctionRturnTypesParsing() throws {
        let testPatterns: [(input: String, expectedTypes: [String])] = [
            ("関数【出力が「数値」で、１。】", ["数値"]),
            ("関数【出力が「文字列か無」で、「い」。】", ["文字列か無"]),
            ("関数【出力が「」】",[""]),
            ("関数【出力が「文字列かエラー」と「数値」で、「い」、1】", ["文字列かエラー", "数値"]),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 1, "statement.expressions.count")
            let function = try XCTUnwrap(statement.expressions.first as? FunctionLiteral)   // 関数であって、
            let functionBlock = function.function
            XCTAssertEqual(functionBlock.returnTypes.count, test.expectedTypes.count, "関数の返り値の数が間違っている。")
            zip(functionBlock.returnTypes, test.expectedTypes).forEach {
                XCTAssertEqual($0, $1)
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
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 1, "statement.expressions.count")
            let function = try XCTUnwrap(statement.expressions.first as? FunctionLiteral)
            let functionBlock = function.function
            XCTAssertEqual(functionBlock.parameters.count, test.expectedParameters.count, "関数のパラメータ数が間違っている。")
            try zip(functionBlock.parameters, test.expectedParameters).forEach {
                try testLiteralExpression($0, with: $1)
            }
            XCTAssertNil(functionBlock.paramForm.values[0]) // x: 既定値無し
            let valueOfy = try XCTUnwrap(functionBlock.paramForm.values[1]?.expressions.first as? IntegerLiteral)
            XCTAssertEqual(valueOfy.value, 1)               // y: 既定値1
            let valueOfz = try XCTUnwrap(functionBlock.paramForm.values[2]?.expressions.first as? IntegerLiteral)
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
            let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 1, "statement.expressions.count")
            let callExpression = try XCTUnwrap(statement.expressions.first as? CallExpression)
            XCTAssertEqual(callExpression.target.tokenLiteral, "加える")
            let args = callExpression.arguments
            XCTAssertEqual(args.count, 2, "parameters.count")
            XCTAssertEqual(args[0].string, "xは、1。")
            XCTAssertEqual(args[1].string, "yは、2。")
            print("テスト終了: \(statement.string)")
        }
    }
    func testProtocolLiteralParsings() throws {
        let testPatterns = [
            "規約であって、【数値は「数値」。文字列は「文字列」。数値化は関数で、入力が文字列「文字列を」。出力が「数値」】",
            "規約であり、数値は「数値」。文字列は「文字列」。数値化は関数で、入力が文字列「文字列を」。出力が「数値」。",
            "規約【数値は「数値」。文字列は「文字列」。数値化は関数で、入力が文字列「文字列を」。出力が「数値」】",
            "規約【数値は「数値」。文字列は「文字列」。数値化は関数【入力が文字列「文字列を」。出力が「数値」】】",
        ]
        for input in testPatterns {
            print("テストパターン: \(input)")
            let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 1, "statement.expressions.count")
            let protocolLiteral = try XCTUnwrap(statement.expressions.first as? ProtocolLiteral)
            XCTAssertEqual(protocolLiteral.clauses.count, 3, "protocolLiteral.clauses.count")
            XCTAssertEqual(protocolLiteral.clauses[0].string.withoutPeriod, "数値は、「数値」")
            XCTAssertEqual(protocolLiteral.clauses[1].string.withoutPeriod, "文字列は、「文字列」")
            XCTAssertEqual(protocolLiteral.clauses[2].string, "数値化は、関数であって、【入力が文字列「文字列を」。出力が「数値」。】。")
            print("テスト終了: \(statement.string)")
        }
    }
    func testTypeLiteralParsings() throws {
        let testPatterns = [
            "型であって、【初期化は、【入力が、xとyであり、xにyを足し、zに代入する】。本体が、aは１。】",
            "型であり、初期化は、【入力が、xとyで、xにyを足し、zに代入する】。本体が、aは１。",
            "型【初期化【入力がxとyで、xにyを足し、zに代入する】。aは１】",
        ]
        for input in testPatterns {
            print("テストパターン: \(input)")
            let program = try XCTUnwrap(parseProgram(with: input, isShadowMode: false, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            XCTAssertEqual(statement.expressions.count, 1, "statement.expressions.count")
            let typeLiteral = try XCTUnwrap(statement.expressions.first as? TypeLiteral)
            let params = try XCTUnwrap(typeLiteral.initializers.single?.parameters)
            XCTAssertEqual(params.count, 2, "typeLiteral.parameters.count")
            try testLiteralExpression(params[0], with: "x")
            try testLiteralExpression(params[1], with: "y")
            let body = try XCTUnwrap(typeLiteral.initializers.single?.body)
            XCTAssertEqual(body.statements.count, 1)
            if useSentenceAST {
                let initStmt = try XCTUnwrap(body.statements.first as? CompoundStatement)
                XCTAssertEqual(initStmt.sentences.count, 2)
                try testPhraseExpression(initStmt.sentences[0].arguments[0], with: "xに")
                try testPhraseExpression(initStmt.sentences[0].arguments[1], with: "yを")
                try testKeywordLiteral(initStmt.sentences[0].predicate, "足し")
                let assignment = try XCTUnwrap(initStmt.sentences[1] as? AssignmentSentence)
                try testIdentifier(assignment.target, value: "z")
                try testDefineStatement(typeLiteral.body?.statements.first, target: "a", with: 1)
                print("テスト修了：\(statement.string)")
                continue
            }
            let initStatement = try XCTUnwrap(body.statements.first as? ExpressionStatement)
            XCTAssertEqual(initStatement.expressions.count, 5)
            try testPhraseExpression(initStatement.expressions[0], with: "x", "に")
            try testPhraseExpression(initStatement.expressions[1], with: "y", "を")
            try testKeywordLiteral(initStatement.expressions[2], "足し")
            try testPhraseExpression(initStatement.expressions[3], with: "z", "に")
            try testKeywordLiteral(initStatement.expressions[4], "代入する")
            XCTAssertNotNil(typeLiteral.body)
            XCTAssertEqual(typeLiteral.body!.statements.count, 1)
            let bodyStatement = try XCTUnwrap(typeLiteral.body!.statements.first as? DefineStatement)
            try testDefineStatement(bodyStatement, target: "a", with: 1)
            print("テスト終了: \(statement.string)")
        }
    }
    func testProtocolComformingProtocols() throws {
        let input = "規約であって、準拠する規約が、甲と、乙と、丙。条項が、丁は「数値」。"
        print("テストパターン: \(input)")
        let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
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
            let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
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
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            let define = try XCTUnwrap(program.statements[0] as? DefineStatement)
            XCTAssertEqual(define.name.value, "列挙子")
            let enumerator = try XCTUnwrap(define.rhsLiteral as? EnumeratorLiteral)
            XCTAssertEqual(enumerator.type, test.type)
            XCTAssertEqual(enumerator.name, test.name)
            print("テスト終了: \(enumerator.string)")
        }
    }
    func testArrayLiterals() throws {
        let input = "配列であって、【要素が、１と、2に２を掛けたものと、3に３を足したもの】。"
        print("テストパターン: \(input)")
        let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
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
        let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
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
            let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            let dictionary = try XCTUnwrap(statement.expressions.first as? DictionaryLiteral)
            XCTAssertEqual(dictionary.pairs.count, 0)
            print("テスト(\(statement.string))終了")
        }
   }
    func testParsingDictionaryLiteralWithExpressions() throws {
        let input = "辞書【「一」が0と1を足す、「二」が10から8を引く、「三」が15を5で割る】"
        print("テストパターン: \(input)")
        let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
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
        let program = try XCTUnwrap(parseProgram(with: input, useSentenceAST: useSentenceAST))
        let statement = try XCTUnwrap(program.statements.first)
        XCTAssertEqual(statement.string, expected)
        print("テスト(\(program.string))終了")
    }
    func testGenitiveExpressions() throws {
        let testPatterns: [(input: String, expected: String)] = [
            ("甲の乙の「一」の１", "甲の乙の「一」の1"),
            ("甲の乙は、１", "甲の乙は、1"),
            ("甲の１番目は、１と２を足す", "甲の1は、1と2を足す"),
            ("甲の。", "甲の"),
            ("""
             甲の
             1。
             """, "甲の"),
            ("甲の", "甲の"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            let statement = try XCTUnwrap(program.statements.first)
            XCTAssertEqual(statement.string.withoutPeriod, test.expected)
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
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, 1, "program.statements.count")
            if useSentenceAST {
                let cs = try XCTUnwrap(program.statements.first as? CompoundStatement)
                XCTAssertEqual(cs.sentences[0].string, "甲が乙より小さい。")
                let le = try XCTUnwrap((cs.sentences[1] as? ExpressionStatement)?.expressions.first as? LogicalExpression)
                XCTAssertEqual(le.tokenLiteral, test.expected)
                XCTAssertEqual(le.right.statements.first?.string, "甲が1に等しい。")
                print("テスト終了: \(cs.string)(\(type(of: cs)))")
                continue
            }
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
                XCTAssertEqual(loop.body.statements.first?.string, "数字を表示する。")
            }),
            ("1000から1まで-1ずつ反復【入力が数字で、数字を表示する】。", { expressions in
                try self.testPhraseExpression(expressions[0], with: 1000, "から")
                try self.testPhraseExpression(expressions[1], with: 1, "まで")
                try self.testIntegerLiteral(expressions[2], value: -1)
                let loop = try XCTUnwrap(expressions[3] as? LoopExpression)
                XCTAssertEqual(loop.token, .keyword(.LOOP))
                try self.testIdentifier(loop.parameters.first, value: "数字")
                XCTAssertNil(loop.condition)
                XCTAssertEqual(loop.body.statements.first?.string, "数字を表示する。")
            }),
            ("配列【１、２、３】を反復【入力が数字で、数字を表示する】。", { expressions in
                let phrase = try XCTUnwrap(expressions[0] as? PhraseExpression)
                let array = try XCTUnwrap(phrase.left as? ArrayLiteral)
                XCTAssertEqual(array.elements.count, 3)
                let loop = try XCTUnwrap(expressions[1] as? LoopExpression)
                XCTAssertEqual(loop.token, .keyword(.LOOP))
                try self.testIdentifier(loop.parameters.first, value: "数字")
                XCTAssertNil(loop.condition)
                XCTAssertEqual(loop.body.statements.first?.string, "数字を表示する。")
            }),
            ("１から反復【1を足し、100より大きい場合、【中止する】】。", { expressions in
                try self.testPhraseExpression(expressions[0], with: 1, "から")
                let loop = try XCTUnwrap(expressions[1] as? LoopExpression)
                XCTAssertEqual(loop.token, .keyword(.LOOP))
                XCTAssertTrue(loop.parameters.isEmpty)
                XCTAssertNil(loop.condition)
                XCTAssertEqual(loop.body.statements.first?.string, "1を足し、100より大きい場合、【中止する】。")
            }),
            ("１から反復【条件が、100より小さい間、1を足す】。", { expressions in
                try self.testPhraseExpression(expressions[0], with: 1, "から")
                let loop = try XCTUnwrap(expressions[1] as? LoopExpression)
                XCTAssertEqual(loop.token, .keyword(.LOOP))
                XCTAssertTrue(loop.parameters.isEmpty)
                XCTAssertEqual(loop.condition?.string, "100より小さい。")
                XCTAssertEqual(loop.body.statements.first?.string, "1を足す。")
            }),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
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
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
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
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            let range = try XCTUnwrap(statement.expressions.first as? RangeLiteral)
            try test.testFunc(range.lowerBound, range.upperBound)
            print("テスト終了: \(statement.string)")
        }
    }
    func testFunctionParamForms() throws {
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
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            let statement = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            let function = try XCTUnwrap(statement.expressions.first as? FunctionLiteral)
            let functionBlock = function.function
            XCTAssertEqual(functionBlock.paramForm.numberOfInputs, test.number)
            XCTAssertEqual(functionBlock.paramForm.formats.first?.type, test.type)
            XCTAssertEqual(functionBlock.paramForm.formats.first?.particle, test.particle)
            XCTAssertEqual(functionBlock.paramForm.formats.first?.threeDots, test.threeDots)
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
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
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
            """,2,2),   // 5. 。EOL, 12. EOL次
            ("関数【甲を表示する】を表示する。",1,2),    // 6. 】次
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
            ("""
                甲は、配列であって、要素が、１、２、３。

            """,1,1),    // 16. 。 EOL EOL
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let program = try XCTUnwrap(parseProgram(with: test.input, useSentenceAST: useSentenceAST))
            XCTAssertEqual(program.statements.count, test.statements, "program.statements.count")
            if let define = program.statements.first as? DefineStatement {
                XCTAssertEqual(define.rhsCount, test.expressions, "statement.expressions.count")
            } else
            if let statement = program.statements.first as? ExpressionStatement {
                XCTAssertEqual(statement.expressions.count, test.expressions, "statement.expressions.count")
            }
            print("テスト終了: \(program.string)")
        }

    }
    /// 「その」に続く属性チェック
    func testItsParsings() throws {
        let testPatterns: [(input: String, literal: String, type: Token.TokenType)] = [
            ("その数", "数", Token.TokenType.ident),
            ("その型", "型", Token.TokenType.keyword(.TYPE)),
            ("そのもの", "エラー", Token.TokenType.keyword(.MONO)),
            ("その「数」", "エラー", Token.TokenType.string),
            ("そのから", "エラー", Token.TokenType.particle(.KARA)),
            ("その１", "エラー", Token.TokenType.int),
            ("その。", "エラー", Token.TokenType.symbol(.PERIOD))
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let parser = Parser(input: test.input)
            let nextToken = parser.nextToken
            if let es = ExpressionStatementParser(parser).parse() as? ExpressionStatement,
               let expr = es.expressions[0] as? PropertyExpression {
                XCTAssertEqual(expr.token.keyword, .ITS)
                XCTAssertEqual(expr.property.literal, test.literal)
            } else {
                XCTAssertEqual("エラー", test.literal)
            }
            XCTAssertEqual(nextToken.type, test.type)
            print("テスト終了: \(test.literal) → \(nextToken.type)")
        }
    }
    func testParseSentences() throws {
        let testPatterns: [(input: String, numberOfSentences: Int?)] = [
            // 式文
            ("aと1。", 0),
            ("1と2を足したもの", 0),
            ("10を2で割ったものを", 0),
            // 単文
            ("1と2を足す。", 1),
            ("10を2で割ったものを、3から1を引いたもので、割る。", 1),
            ("aに１を加算したものを、表示する。", 1),
            ("aに１を加算", 1),
            ("aでテストを実行したものを、表示する。", 1),
            ("aに1を代入", 1),
            ("1 2 足す。", 1),
            ("真、aに代入する。", 1),
            ("「い」 aに代入する。", 1),
            // 複文
            ("1と2を足し、3を引く。", 2),
            ("3と2を足し1を引く。", 2),
            ("1と2を足して、3を引く。", 2),
            ("1と2を足し、3と4。", 2),
            ("10を2で割って、5を引く。", 2),
            ("1と２を足し、テストを実行。", 2),
            ("aに１を加わえ、表示する。", 2),
            ("aに１を加算し、表示する。", 2),
            ("aに１を加算して、表示する。", 2),
            ("aでテストを実行し、表示する。", 2),
            ("aでテストを実行して、表示する。", 2),
            ("aと1を足し、bに代入。", 2),
            ("1をaに代入し、2をbに代入する。", 2),
            ("aと1を足して、代入する。", 2),
            // エラー
            ("1と2を足し。", nil),
            ("1と2を足す、3を足す。", nil),
            ("1と2を足す、3を引く、4を足し。", nil),
            ("１を代入", nil),
            ("a b cに代入する。", nil),
            ("aを表示して、代入する。", nil),
            ("aを代入", nil),
            ("aに１を代入して、bに代入する。", nil),
            ("aに１を足して代入し、bに１を代入する。", nil),
            ("aに１を代入し、bに１を足して代入する。", nil),
            ("aと1を足し、代入する。", nil),
            ("真かによって、1か2。", nil),
        ]
        for test in testPatterns {
            print("テストパターン： \(test.input)")
            let parser = Parser(input: test.input)
            let program = try XCTUnwrap(parseProgram(with: test.input, isShadowMode: false))
            let es = try XCTUnwrap(program.statements.first as? ExpressionStatement)
            let esp = ExpressionStatementParser(parser)
            guard let stmt = esp.parseSentecne(from: es) else {
                parser.errors.forEach {print($0)}
                XCTAssertNil(test.numberOfSentences, "テスト失敗(エラー発生！！！)")
                print("テスト終了： エラー\(parser.errors.count)個。")
                continue
            }
            var count = 0
            var type: String
            if let compound = stmt as? CompoundStatement {
                count = compound.sentences.count
                type = "複文"
            } else
                if stmt is SimpleSentence || stmt is AssignmentSentence {
                count = 1
                type = "単文"
            } else {
                count = 0
                XCTAssert(stmt is ExpressionStatement)
                type = "式文"
            }
            XCTAssertEqual(count, test.numberOfSentences)
            print("テスト終了： 単文数 ＝ \(count)、型 = \(type)「\(stmt.string)」")
        }
    }
    func testSentenceTreminators() throws {
        let testPatterns: [(input: String, SentenceTerminator: SentenceTerminator?)] = [
            ("1と2を足す。", .period),
            ("1と2を足し。", .period),
            ("1と2を足す】", .rbbracket),
            ("1と2を足し】", .rbbracket),
            ("1と2を足し。】", .period),
            ("1と2を足し】。", .rbbracket),
            ("1と2を足し、", .eof),
            ("1と2を足し", .eof),
            ("1と2を足す、", .eof),
            ("1と2を足す", .eof),
            ("""
            1と2を足す
            
            """, .eol),
            ("1と2を足し、。", nil),
            ("1と2を足し、】", nil),
        ]
        for test in testPatterns {
            print("テストパターン： \(test.input)")
            var terminator: SentenceTerminator? = nil
            let parser = Parser(input: test.input)
            parser.options.useSentenceAST = false
            var string = ""
            if let es = ExpressionStatementParser(parser).parse() as? ExpressionStatement {
                terminator = es.terminator
                string = es.string
            }
            XCTAssertEqual(terminator, test.SentenceTerminator)
            let symbol = (terminator?.rawValue ?? "nil(エラー)")
                .replacingOccurrences(of: "\n", with: "EOL")
                .replacingOccurrences(of: "\0", with: "EOF")
            print("テスト終了： 終端記号 ＝ \(symbol)(\(string))")
        }
    }
    // MARK: - ヘルパー
    private func testDefineStatement(_ statement: Statement?, target: String, with value: Any, flag: Bool = false) throws {
        let defStatement = try XCTUnwrap(statement as? DefineStatement)
        XCTAssertEqual(defStatement.name.value, target)
        XCTAssertEqual(defStatement.name.tokenLiteral, target)
        try testLiteralExpression(defStatement.rhsLiteral, with: value)
        XCTAssertEqual(defStatement.isExtended, flag)
    }
    private func testPhraseExpression(_ expression: Expression,  with expected: Any, _ particle: String) throws {
        let p = try XCTUnwrap(expression as? PhraseExpression)
        XCTAssertEqual(p.tokenLiteral, particle)
        try testLiteralExpression(p.left, with: expected)
    }
    private func testPhraseExpression(_ expression: Expression,  with expected: String) throws {
        let p = try XCTUnwrap(expression as? PhraseExpression)
        XCTAssertEqual(p.string, expected)
    }
    private func testParticleLiteral(_ expression: Expression, _ particle: String) throws {
        let p = try XCTUnwrap(expression as? PhraseExpression)
        XCTAssertEqual(p.string, particle)
    }
    private func testKeywordLiteral(_ expression: Expression?, _ keyword: String) throws {
        let predicate = try XCTUnwrap(expression as? PredicateExpression)
        XCTAssertEqual(predicate.string, keyword)
    }
    private func testKeyword(_ expression: Expression?, _ keyword: Token.Keyword) throws {
        let predicate = try XCTUnwrap(expression as? PredicateExpression)
        XCTAssertNotNil(predicate.token.isKeyword(keyword))
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
    private func testSimpleStatement(_ statement: Statement, predicate: String, phrases: String...) throws {
        let s = try XCTUnwrap(statement as? Sentence)
        XCTAssertEqual(s.arguments.count, phrases.count, "phrases.count")
        for (argument, phrase) in zip(s.arguments, phrases) {
            XCTAssertEqual(argument.string, phrase)
        }
    }
    private func testSimpleSentence(_ ss: Sentence?, predicate: String, phrases: String...) throws {
        let s = try XCTUnwrap(ss as? SimpleSentence)
        XCTAssertEqual(s.predicate?.string, predicate)
        XCTAssertEqual(s.arguments.count, phrases.count, "phrases.count")
        for (argument, phrase) in zip(s.arguments, phrases) {
            XCTAssertEqual(argument.string, phrase)
        }
    }
    private func testCaseConsequence(_ exp: Expression, with str: String) throws {
        let ce = try XCTUnwrap(exp as? CaseExpression)
        let consequence = try XCTUnwrap(ce.consequence.statements.first)
        XCTAssertEqual(consequence.string.withoutComma.withoutPeriod, str)
    }
    private func testCaseAlternative(_ exp: Expression, with str: String) throws {
        let ce = try XCTUnwrap(exp as? CaseExpression)
        let alternative = try XCTUnwrap(ce.alternative?.statements.first)
        XCTAssertEqual(alternative.string.withoutPeriod, str)
    }
}
