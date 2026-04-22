//
//  構文解析器
//  parser.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/02/21.
//
import Foundation

final class Parser {
    // MARK: 変数メンバーとその初期化
    init(_ lexer: Lexer) {
        self.lexer = lexer
        previousToken = Lexer.EoT           // 前のトークン
        currentToken = lexer.getNext()      // カレントトークン
        nextToken =    lexer.getNext()      // 次のトークン
    }
    convenience init(input: String) {       // テスト用
        self.init(Lexer(input))
    }
    /// コピーコンストラクタ
    init(from other: Parser) {
        self.lexer = Lexer(from: other.lexer)
        self.currentToken = other.currentToken
        self.nextToken = other.nextToken
        self.previousToken = other.previousToken
        self.blockStack = other.blockStack
        self.switchCase = other.switchCase
        self.isInRangeParser = other.isInRangeParser
        self.errors = other.errors
    }
    //
    let lexer: Lexer
    var currentToken: Token
    var nextToken: Token
    var previousToken: Token
    var errors: [String] = []
    var blockStack: [BlockFrame] = []
    var switchCase = SwitchCase()           // Switch-case監視
    var isInRangeParser: Bool = false
    var leadingIdentifier: Identifier? = nil
    var options = ParserOptions()
    // MARK: - プログラムの解析
    func parseProgram() -> Program? {
        var statements: [Statement] = []
        switchCase.enter()
        while !currentToken.isEof {
            skipEols()
            guard let statement = StatementParserFactory.create(from: self).parse() else {return nil}
            statements.append(statement)
            while getNext(whenNextIs: Token.symbol(.EOL)) {}
            getNext()
        }
        if switchCase.isActive {
            errors.append(switchCase.defaultError)
            return nil
        }
        switchCase.leave()
        return Program(statements: statements)
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
    /// expetcted文字列に、tokenが一致していれば解析位置を進める。
    /// - Parameters:
    ///   - expected: チェックする文字列（揺れを許す場合は、"である" + "であって"のようにすれば、期待する位置まで解析位置を進めることができる。)
    ///   - matchAll: true(デフォルト)の場合、揺れを許容せず完全一致しない場合は、falseを返す。
    /// - Returns: matchAll = falseの場合、常にtrueを返す。
    func getNext(whenNextIs expected: String, matchAll: Bool = true) -> Bool {
        let lexer = Lexer(expected)
        var tokens: [Token] = []
        var token = lexer.getNext()
        while token != Lexer.EoT {  // lexerからtokensを取り出す
            tokens.append(token)
            token = lexer.getNext()
        }
        // コピーしたParserで、仮の解析をする。(途中で失敗したらfalse)
        let tempParser = Parser(from: self)
        for token in tokens {
            guard tempParser.getNext(whenNextIs: token) || !matchAll else {return false}
        }
        // 一致した場合、実際の解析を進める。
        tokens.forEach {_ = getNext(whenNextIs: $0)}
        return true
    }
    func getNext(whenNextIs expected: Token.Symbol, withError: Bool = false) -> Bool {getNext(whenNextIs: Token.symbol(expected), withError: withError)}
    func skipEols() {while currentToken.isEol {getNext()}}
    /// Lexerに識別子を登録
    func insert(_ identifier: String) {lexer.insert(identifier)}
}
/// ブロック管理
enum BlockKind {
    case explicit
    case implicit
    //
    init(isExplicit: Bool) {
        self = isExplicit ? .explicit : .implicit
    }
}
struct BlockFrame {
    let kind: BlockKind
    let target: String      // ブロックの対象、トークン
    //
    init(kind: BlockKind, token: Token?) {
        self.kind = kind
        self.target = token?.literal ?? ""
    }
    init(kind: BlockKind, target: String) {
        self.kind = kind
        self.target = target
    }
}
// MARK: - Helpers for Unit Test
func parseProgram(with input: String) -> Program? {
    let lexer = Lexer(input)
    let parser = Parser(lexer)
    parser.options.useSentenceAST = true
    let program = parser.parseProgram()
    check(parser.errors)
    return program
}
private func check(_ errors: [String]) {
    if errors.isEmpty {return}
    print("Parserが、\(errors.count)個のエラーを検出しました。")
    errors.forEach {print("Parerエラー: \($0)")}
}
