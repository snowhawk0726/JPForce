//
//  構文解析器
//  parser.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/02/21.
//
import Foundation

class Parser {
    // MARK: 変数メンバーとその初期化
    init(_ lexer: Lexer) {
        self.lexer = lexer
        previousToken = Lexer.EoT           // 前のトークン
        currentToken = lexer.getNext()      // カレントトークン
        nextToken =    lexer.getNext()      // 次のトークン
    }
    /// コピーコンストラクタ
    init(from other: Parser) {
        self.lexer = Lexer(from: other.lexer)
        self.currentToken = other.currentToken
        self.nextToken = other.nextToken
        self.previousToken = other.previousToken
        self.errors = other.errors
        self.nestedBlockCounter = other.nestedBlockCounter
        self.nestedElementsCounter = other.nestedElementsCounter
        self.switchCase = other.switchCase
    }
    //
    let lexer: Lexer
    var currentToken: Token
    var nextToken: Token
    var previousToken: Token
    var errors: [String] = []
    var nestedBlockCounter = NestCounter(.RBBRACKET, .EOL)
    var nestedElementsCounter = NestCounter(.RBBRACKET, .PERIOD)
    var switchCase = SwitchCase()           // Switch-case監視
    // MARK: - プログラムの解析
    func parseProgram() -> Program? {
        let program = Program()
        switchCase.enter()
        while !currentToken.isEof {
            skipEols()
            guard let statement = StatementParserFactory.create(from: self).parse() else {return nil}
            program.statements.append(statement)
            while getNext(whenNextIs: Token.symbol(.EOL)) {}
            getNext()
        }
        if switchCase.isActive {
            errors.append(switchCase.defaultError)
            return nil
        }
        switchCase.leave()
        return program
    }
    // MARK: - ヘルパー
    // トークン解析制御
    /// 解析位置を進める。
    func getNext() {
        previousToken = currentToken
        currentToken = nextToken
        nextToken = lexer.getNext()
    }
    /// nextTokenがexptectedのトークンと一致している場合、解析位置を進める。
    /// - Parameters:
    ///   - expected: チェックするToken（連想値も含めチェックする。）
    ///   - withError: trueの場合、エラーを出力
    /// - Returns: チェック結果を返す。
    func getNext(whenNextIs expected: Token, withError: Bool = false) -> Bool {
        guard nextToken == expected else {
            if withError {errors.append("次のトークンは「\(expected)」であるはずだが、「\(nextToken)」だった。")}
            return false
        }
        getNext()
        return true
    }
    /// nextToken.typeがexptectedのTokenTypeと一致している場合、解析位置を進める。
    /// - Parameters:
    ///   - expected: チェックするTokenType (IDENT等の連想値はチェックしない)
    ///   - withError: trueの場合、エラーを出力
    /// - Returns: チェック結果を返す。
    func getNext(whenNextIs expected: Token.TokenType, withError: Bool = false) -> Bool {
        guard nextToken.type == expected else {
            if withError {errors.append("次のトークンは「\(expected)」であるはずだが、「\(nextToken)」だった。")}
            return false
        }
        getNext()
        return true
    }
    func skipEols() {while currentToken.isEol {getNext()}}
    /// Lexerに識別子を登録
    func insert(_ identifier: String) {lexer.insert(identifier)}
    /// 入れ子制御カウンター
    class NestCounter {
        var counters: [Token.Symbol : Int] = [:]
        init(_ counters: Token.Symbol...) {counters.forEach {self.counters[$0] = 0}}
        private init() {}
        //
        func up(to symbol: Token.Symbol) {counters[symbol]! += 1}
        func down(to symbol: Token.Symbol) {counters[symbol]! -= 1}
        func value(of symbol: Token.Symbol) -> Int {counters[symbol]!}
    }
}
// MARK: - Helpers for Unit Test
func parseProgram(with input: String) -> Program? {
    let lexer = Lexer(input)
    let parser = Parser(lexer)
    let program = parser.parseProgram()
    check(parser.errors)
    return program
}
private func check(_ errors: [String]) {
    if errors.isEmpty {return}
    print("Parserが、\(errors.count)個のエラーを検出した。")
    errors.forEach {print("Parerエラー: \($0)")}
}
