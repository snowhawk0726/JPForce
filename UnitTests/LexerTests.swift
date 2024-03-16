//
//  LexerTests.swift
//  UnitTests
//
//  Created by 佐藤貴之 on 2023/02/17.
//

import XCTest

final class LexerTests: XCTestCase {
    override func setUpWithError() throws {
    }
    override func tearDownWithError() throws {
    }
    func testNextSymbol() throws {
        let input = "（コメント）「文字列 」。、" + ",『識別子』-−\n\t\r (\"コメント。\")\0※"
        print("テストパターン: \(input)")
        let testPatterns: [(expectedType: Token, expectedLiteral: String)] = [
            (Token.STRING("文字列 "), "文字列 "),
            (Token.symbol(.PERIOD), "。"),
            (Token.symbol(.COMMA), "、"),
            (Token.symbol(.COMMA), "、"),
            (Token.IDENT("識別子"), "識別子"),
            (Token.symbol(.MINUS), "-"),
            (Token.symbol(.MINUS), "-"),
            (Token.symbol(.EOL), "\n"),
            (Token.symbol(.EOF), "\0"),
        ]
        let lexer = Lexer(input)
        print("tagged input: ", terminator: "")
        lexer.enumerated.forEach {print($0 + ($0 != " " ? "(\($1))" : ""), terminator: "")}
        print()
        print("Token.symbols: ", terminator: "")
        Token.symbols.forEach { pair in
            let ch = ["\0":"\\0","\n":"\\n","\r":"\\r","\t":"\\t",]
                .reduce(pair.key) {$0.replacingOccurrences(of: $1.key, with: $1.value)}
            print("<\(ch).\(pair.value)>", terminator: ", ")
        }
        print()
        for test in testPatterns {
            let token = lexer.getNext()
            XCTAssertEqual(token, test.expectedType)
            XCTAssertEqual(token.literal, test.expectedLiteral)
            if token == test.expectedType {
                print("テスト(\(token.literal))成功: ")
            }
        }
    }
    func testNextParticle() throws {
        let input = "は、が、の、に、を、と、とは、から、より、で、だ。た、て、では。"
        print("テストパターン: \(input)")
        let testPatterns: [(expectedType: Token.TokenType, expectedLiteral: String)] = [
            (.particle(.WA), "は"), (.symbol(.COMMA), "、"),
            (.particle(.GA), "が"), (.symbol(.COMMA), "、"),
            (.particle(.NO), "の"), (.symbol(.COMMA), "、"),
            (.particle(.NI), "に"), (.symbol(.COMMA), "、"),
            (.particle(.WO), "を"), (.symbol(.COMMA), "、"),
            (.particle(.TO), "と"), (.symbol(.COMMA), "、"),
            (.particle(.TOWA), "とは"), (.symbol(.COMMA), "、"),
            (.particle(.KARA), "から"), (.symbol(.COMMA), "、"),
            (.particle(.YORI), "より"), (.symbol(.COMMA), "、"),
            (.particle(.DE), "で"), (.symbol(.COMMA), "、"),
            (.particle(.DA), "だ"), (.symbol(.PERIOD), "。"),
            (.particle(.TA), "た"), (.symbol(.COMMA), "、"),
            (.particle(.TA), "て"), (.symbol(.COMMA), "、"),
            (.particle(.DEWA), "では"), (.symbol(.PERIOD), "。"),
        ]
        let lexer = Lexer(input)
        print("tagged input: ", terminator: "")
        lexer.enumerated.forEach {print($0 + ($0 != " " ? "(\($1))" : ""), terminator: "")}
        print()
        for test in testPatterns {
            let token = lexer.getNext()
            XCTAssertEqual(token.type, test.expectedType)
            XCTAssertEqual(token.literal, test.expectedLiteral)
            print("テスト(\(token.type)：「\(token.literal)」)終了")
        }
    }
   func testNextToken() throws {
        let input = """
            (「\\(コメント化)」)
            2に3を足す。
            10から3を引く。
            最低温度（「括弧内」はコメント）は、５度。※ ５は全角
            気温が、最低気温以上、最高気温以下の場合、
            二倍とは、２を掛けること。※ 米印から行末までコメント。２は全角。ふぉーすでは、エラーになる文
            2を二倍し、3で割る。
            絶対防空圏内（３語以上の合成識別子を確認する。）
            「こんにちは 」
            「今日は、みなさん。」
            入力が、10未満の場合、真を返し、それ以外は、偽を返す。
            10は、10に等しい。
            10は、5に等しくない。
            変数に、２を足し、３を引き、４を掛け、５で割る。
            入力が、甲と乙であり、
            1と文字１を入力画面に入力する。
            200m先に、鳥が２羽（単位は無視する）
            二次関数の場合、
            『割った余り』
            「foobar」
            「foo bar」
            3.14
            1,260
            １、２、３
        
        """
        print("テストパターン: \(input)")
        let testPatterns: [(expectedType: Token, expectedLiteral: String)] = [
            (Token(.EOL),"\n"),
            (.INT(),"2"),(Token(.NI),"に"),(.INT(),"3"),(Token(.WO),"を"),(Token(.ADD),"足す"),(Token(.PERIOD),"。"),(Token(.EOL),"\n"),
            (.INT(),"10"),(Token(.KARA),"から"),(.INT(),"3"),(Token(.WO),"を"),(Token(.SUBSTRACT),"引く"),(Token(.PERIOD),"。"),(Token(.EOL),"\n"),
            (.IDENT(), "最低温度"),(Token(.WA),"は"),(Token(.COMMA),"、"),(.INT(),"5"),(Token(.PERIOD),"。"),(Token(.EOL),"\n"),
            (.IDENT(), "気温"),(Token(.GA),"が"),(Token(.COMMA),"、"),(.IDENT(),"最低気温"),(Token(.GTEQUAL),"以上"),(Token(.COMMA),"、"),(.IDENT(),"最高気温"),(Token(.LTEQUAL),"以下"),(Token(.NO),"の"),(Token(.CASE),"場合"),(Token(.COMMA),"、"),(Token(.EOL),"\n"),
            (.IDENT(),"二倍"),(Token(.TOWA),"とは"),(Token(.COMMA),"、"),(.INT(),"2"),(Token(.WO),"を"),(Token(.MULTIPLY),"掛ける"),/*(Token(.KOTO),"こと"),*/(Token(.PERIOD),"。"),(Token(.EOL),"\n"),
            (.INT(),"2"),(Token(.WO),"を"),(.IDENT(),"二倍"),(Token(.SURU),"し"),(Token(.COMMA),"、"),(.INT(),"3"),(Token(.DE),"で"),(Token(.DIVIDE),"割る"),(Token(.PERIOD),"。"),(Token(.EOL),"\n"),
            (.IDENT(),"絶対防空圏内"),(Token(.EOL),"\n"),
            (.STRING(),"こんにちは "),(Token(.EOL),"\n"), (.STRING(),"今日は、みなさん。"),(Token(.EOL),"\n"),            (Token(.INPUT),"入力"),(Token(.GA),"が"),(Token(.COMMA),"、"),(.INT(),"10"),(Token(.UNDER),"未満"),(Token(.NO),"の"),(Token(.CASE),"場合"),(Token(.COMMA),"、"),(Token(.TRUE),"真"),(Token(.WO),"を"),(Token(keyword: .RETURN),"返し"),(Token(.COMMA),"、"),(.IDENT(),"それ以外"),(Token(.WA),"は"),(Token(.COMMA),"、"),(Token(.FALSE),"偽"),(Token(.WO),"を"),(Token(keyword: .RETURN),"返す"),(Token(.PERIOD),"。"),(Token(.EOL),"\n"),
            (.INT(),"10"),(Token(.WA),"は"),(Token(.COMMA),"、"),(.INT(),"10"),(Token(.NI),"に"),(Token(.EQUAL),"等しい"),(Token(.PERIOD),"。"),(Token(.EOL),"\n"),
            (.INT(),"10"),(Token(.WA),"は"),(Token(.COMMA),"、"),(.INT(),"5"),(Token(.NI),"に"),(Token(.EQUAL),"等しく"),(Token(.NOT),"ない"),(Token(.PERIOD),"。"),(Token(.EOL),"\n"),
            (.IDENT(),"変数"),(Token(.NI),"に"),(Token(.COMMA),"、"),(.INT(),"2"),(Token(.WO),"を"),(Token(.ADD),"足し"),(Token(.COMMA),"、"),(.INT(),"3"),(Token(.WO),"を"),(Token(.SUBSTRACT),"引き"),(Token(.COMMA),"、"),(.INT(),"4"),(Token(.WO),"を"),(Token(.MULTIPLY),"掛け"),(Token(.COMMA),"、"),(.INT(),"5"),(Token(.DE),"で"),(Token(.DIVIDE),"割る"),(Token(.PERIOD),"。"),(Token(.EOL),"\n"),
            (Token(.INPUT),"入力"),(Token(.GA),"が"),(Token(.COMMA),"、"),(.IDENT(),"甲"),(Token(.TO),"と"),(.IDENT(),"乙"),(Token(.DE),"で"),(Token(.BE),"あり"),(Token(.COMMA),"、"),(Token(.EOL),"\n"),
            (.INT(),"1"),(Token(.TO),"と"),(.IDENT(),"文字1"),(Token(.WO),"を"),(.IDENT(),"入力画面"),(Token(.NI),"に"),(Token(.INPUT),"入力"),/*(Token(.SURU),"する"),*/(Token(.PERIOD),"。"),(Token(.EOL),"\n"),
            (.INT(),"200"),(Token(.NI),"に"),(Token(.COMMA),"、"),(.IDENT(),"鳥"),(Token(.GA),"が"),(.INT(),"2"),(Token(.EOL),"\n"),
            (.IDENT(),"二次関数"),(Token(.NO),"の"),(Token(.CASE),"場合"),(Token(.COMMA),"、"),(Token(.EOL),"\n"),
            (.IDENT(),"割った余り"),(Token(.EOL),"\n"),
            (.STRING(),"foobar"),(Token(.EOL),"\n"),(.STRING(),"foo bar"),(Token(.EOL),"\n"),
            (.IDENT(),"3.14"),(Token(.EOL),"\n"),(.INT(),"1"),(Token(.COMMA),"、"),(.INT(),"260"),(Token(.EOL),"\n"),
            (.INT(),"1"),(Token(.COMMA),"、"),(.INT(),"2"),(Token(.COMMA),"、"),(.INT(),"3"),(Token(.EOL),"\n"),
        ]
        let lexer = Lexer(input)
        print("tagged input: ", terminator: "")
        lexer.enumerated.forEach {print($0 + ($0 != " " ? "(\($1))" : ""), terminator: "")}
        print()
        for test in testPatterns {
            let token = lexer.getNext()
            XCTAssertEqual(token.type, test.expectedType.type)
            XCTAssertEqual(token.literal, test.expectedLiteral)
            if token.type == test.expectedType.type &&
               token.literal == test.expectedLiteral {
                print("テスト(\(token.literal))成功")
            }
        }
    }
    func testNumberAndUnits() throws {
        let testPatterns: [(input: String, expected: Int)] = [
            ("100個", 100), ("2番目", 2), ("33平米", 33), ("9光年先", 9),
            ("2km/h", 2), ("33m3", 33),
            ("9", 9), ("-10", -10), ("-25ns", -25),
        ]
        for test in testPatterns {
            print("テスト(\(test.input))開始")
            let lexer = Lexer(test.input)
            let token = lexer.getNext()
            XCTAssertEqual(token.number, test.expected)
            print("テスト(\(token.literal))終了")
       }
    }
}
