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
    let lexer: Lexer
    var currentToken: Token
    var nextToken: Token
    var previousToken: Token
    var errors: [String] = []
    var blockCounter = 0
    // MARK: - プログラムの解析
    func parseProgram() -> Program? {
        var program = Program()
        while !currentToken.isEof {
            skipEols()
            guard let statement = StatementParserFactory.create(from: self).parse() else {return nil}
            program.statements.append(statement)
            while getNext(whenNextIs: Token.symbol(.EOL)) {}
            getNext()
        }
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
    func increment() {blockCounter += 1}
    func decrement() {blockCounter -= 1}
}
