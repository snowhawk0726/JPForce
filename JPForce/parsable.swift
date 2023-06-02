//
//  parsable.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/02/21.
//
import Foundation

// MARK: - intefaces
protocol Parsable {
    var parser: Parser {get}
}
protocol StatementParsable : Parsable {
    func parse() -> Statement?
}
protocol ExpressionParsable : Parsable {
    func parse() -> Expression?
}
// MARK: - default implements for Parsable implementers (Parser delegation)
extension Parsable {
    // トークン読み出し
    var currentToken: Token {parser.currentToken}
    var nextToken: Token    {parser.nextToken}
    // 解析位置制御
    func getNext()          {parser.getNext()}
    func getNext(whenNextIs expected: Token, withError: Bool = false) -> Bool {parser.getNext(whenNextIs: expected, withError: withError)}
    func getNext(whenNextIs expected: Token.TokenType, withError: Bool = false) -> Bool {parser.getNext(whenNextIs: expected, withError: withError)}
    func getNext(whenNextIs expected: Token.Symbol,   withError: Bool = false) -> Bool {getNext(whenNextIs: Token.symbol(expected), withError: withError)}
    func getNext(whenNextIs expected: Token.Keyword,  withError: Bool = false) -> Bool {getNext(whenNextIs: Token.keyword(expected), withError: withError)}
    func getNext(whenNextIs expected: Token.Particle, withError: Bool = false) -> Bool {getNext(whenNextIs: Token.particle(expected), withError: withError)}
    func getNext(whenNextAre expecteds: Token.Symbol...) {
        expecteds.forEach {_ = getNext(whenNextIs: $0)}
    }
    /// expetcted文字列に、tokenが一致していれば解析位置を進める。
    /// - Parameters:
    ///   - expected: チェックする文字列（揺れを許す場合は、"である" + "であって"のようにすれば、期待する位置まで解析位置を進めることができる。)
    ///   - matchAll: true(デフォルト)の場合、揺れを許容せず完全一致しない場合は、falseを返す。
    /// - Returns: matchAll = falseの場合、常にtrueを返す。
    func getNext(whenNextIs expected: String, matchAll: Bool = true) -> Bool {
        let lexer = Lexer(expected)
        var token = lexer.getNext()
        while token != Lexer.EoT {
            guard getNext(whenNextIs: token) || !matchAll else {return false}
            token = lexer.getNext()
        }
        return true
    }
    func skipEols() {parser.skipEols()}
    func skipNextEols() {while nextToken.isEol {getNext()}}
    // 判定
    /// 解析中止要因：】, EOF, EOL, 。
    var isBreakFactor: Bool {
        isEndOfBlock || currentToken.isEof || currentToken.isEol || isEndOfStatement
    }
    var isEndOfStatement: Bool {currentToken.isPeriod}
    var isEndOfBlock: Bool {currentToken == .symbol(.RBBRACKET)}
    var isEndOfElements: Bool {
        nextToken.type == .symbol(.COMMA) || nextToken.type == .symbol(.PERIOD) ||
        nextToken.type == .symbol(.RBBRACKET) ||
        nextToken.type == .symbol(.EOL) || nextToken.type == .symbol(.EOF)
    }
    /// エラー出力
    func error(message: String) {parser.errors.append(message + "(解析位置:\(currentToken.literal))")}
    /// ブロックカウンター制御
    func counter(_ symbol: Token.Symbol) -> Int {parser.counters[symbol]!}
    func countUp(_ symbol: Token.Symbol) {parser.counters[symbol]! += 1}
    func countDown(_ symbol: Token.Symbol) {parser.counters[symbol]! -= 1}
    /// 入力部の解析
    /// - 形式: 入力が、識別子1（「型格」）と 識別子2（「型格」）と...、であり、
    func parseParameters(endSymbol: Token.Symbol) -> [(Identifier, String)]? {
        guard getNext(whenNextIs: ExpressionStatement.input) else {return []}  // 空のパラメータ
        _ = getNext(whenNextIs: ExpressionStatement.ga + ExpressionStatement.wa, matchAll: false)   // 入力が、(入力は、)
        _ = getNext(whenNextIs: .COMMA)
        var parameters: [(Identifier, String)] = []
        repeat {
            getNext()
            let identifier = Identifier(from: currentToken)     // 識別子
            let format = getNext(whenNextIs: .string) ? currentToken.literal : ""   // 入力形式
            parameters.append((identifier, format))
            _ = getNext(whenNextIs: .TO)                        // と
            guard nextToken != .symbol(endSymbol) && !nextToken.isEof else {return nil}
        } while !(getNext(whenNextIs: .DE) ||
                  getNext(whenNextIs: .COMMA))                  // で or 、
        _ = getNext(whenNextIs: ExpressionStatement.ari)        // (あり)
        _ = getNext(whenNextIs: .COMMA)                         // (、)
        return parameters
    }
    func parseSignature(from strings: [String]) -> InputFormat {
        let threeDots = "…"
        let formats = strings.map { string in
            var type = "", particle = ""
            let lexer = Lexer(string)
            var token = lexer.getNext()
            if token.isIdent || token.isKeyword {type = token.literal;token = lexer.getNext()}
            if token.isParticle {particle = token.literal;token = lexer.getNext()}
            if token.literal == threeDots {particle += token.literal}
            return (type, particle)
        }
        let number = formats.map({$0.1}).contains {$0.hasSuffix(threeDots)} ? nil : strings.count
        return InputFormat(numberOfInputs: number, formats: formats)
    }
    /// 式の解析　(例：1以上→範囲【1以上】)
    /// - Parameter expression: 式(数値、識別子)
    /// - Returns: 上限もしくは下限の範囲リテラル(もしくは元の式)
    func parseRangeExpression(with expression: Expression) -> Expression? {
        let keyword = nextToken
        switch keyword {
        case .keyword(.GTEQUAL):
            getNext()
            return RangeLiteral(token: .keyword(.RANGE), lowerBound: ExpressionStatement(token: keyword, expressions: [expression]))
        case .keyword(.LTEQUAL),.keyword(.UNDER):
            getNext()
            return RangeLiteral(token: .keyword(.RANGE), upperBound: ExpressionStatement(token: keyword, expressions: [expression]))
        default:
            return expression
        }
    }
    /// 式の配列の解析(上下限を範囲リテラルとして切り出す)
    /// - Parameters:
    ///   - expressions: 範囲式
    ///   - token: 「範囲」トークン
    /// - Returns: 範囲リテラル
    func parseRangeExpressions(_ expressions: [Expression], token: Token) -> Expression? {
        let lowerBound = getBound(of: [Token(.KARA), Token(.GTEQUAL)], from: expressions)
        let rest = getRest(of: expressions, except: lowerBound)
        guard rest.isEmpty || getBound(of: [Token(.KARA), Token(.GTEQUAL)], from: rest) == nil else {
            error(message: "範囲で、範囲式の解析に失敗した。(下限「\(lowerBound?.tokenLiteral ?? "?")」が重複)")
            return nil
        }
        let upperBound = getBound(of: [Token(.MADE), Token(.LTEQUAL), Token(.UNDER)], from: rest)
        if upperBound != nil {
            guard getRest(of: rest, except: upperBound).isEmpty else {
                error(message: "範囲で、範囲式の解析に失敗した。(上限「\(upperBound?.tokenLiteral ?? "?")」に後続の式がある。)")
                return nil
            }
        }
        return RangeLiteral(token: token, lowerBound: lowerBound, upperBound: upperBound)
    }
    /// 解析された式の配列から、上限もしくは下限(キーワードと式)を抽出する。
    /// *1* 「<数値>(または<識別子>)キーワード」は、範囲【<数値>(または<識別子>)キーワード】と解析されている。
    /// *2* 「<式>から」は、カラ句に解析されている。
    /// *3* 「<式>まで」は、マデ句に解析されている。
    /// - Parameters:
    ///   - tokens: 上下限のトークン(*: 先頭は格「から」または「まで」)
    ///   - expressions: 式の配列
    /// - Returns: 抽出した上限もしくは下限(式文)
    private func getBound(of tokens: [Token], from expressions: [Expression]) -> ExpressionStatement? {
        // 式がIntegerLiteralで、tokenが、RangeLiteralまたはPhraseExpressionに解析済みの場合
        if let e = expressions.first as? RangeLiteral {return isLowBound(tokens.first) ? e.lowerBound : e.upperBound}
        if let e = expressions.first as? PhraseExpression, e.token == tokens.first {return ExpressionStatement(token: e.token, expressions: [e.left])}
        // 複数式から、キーワードを拾いだす。(index.0: 下限キーワード、index.1: 拾いだした式の位置)
        guard let index = firstIndex(of: expressions, by: tokens) else {return nil}
        var rangeExpressions = [Expression](expressions[0..<index.1])
        if index.0 == tokens.first {
            if let p = expressions[index.1] as? PhraseExpression {rangeExpressions.append(p.left)}
        }
        return ExpressionStatement(token: index.0, expressions: rangeExpressions)
    }
    /// expressionsから、es部分を除いた残りを返す。
    /// - Parameters:
    ///   - expressions: 入力の式配列
    ///   - es: 下限部
    /// - Returns: 残りの式配列
    private func getRest(of expressions: [Expression], except es: ExpressionStatement?) -> [Expression] {
        guard let es = es else {return expressions}
        var position = es.expressions.count
        guard position < expressions.count else {return []}
        if let e = expressions[position] as? PredicateExpression, e.token == es.token {
            position += 1
        }
        return [Expression](expressions[position..<expressions.count])
    }
    /// 上下限キーワードの位置を返す。
    /// - Parameters:
    ///   - expressions: 対象の式配列
    ///   - tokens: 検索するキーワード配列
    /// - Returns: ０： 検出したキーワード(Token)、１：検出した位置
    private func firstIndex(of expressions: [Expression], by tokens: [Token]) -> (Token, Int)? {
        for t in tokens {
            if let i = expressions.firstIndex(where: {
                if let p = $0 as? PredicateExpression {return p.token == t} // 以上、以下、未満
                if let p = $0 as? PhraseExpression {return p.token == t}    // から、まで
                return false
            }) {
                return (t, i)
            }
        }
        return nil
    }
    private func isLowBound(_ token: Token?) -> Bool {[Token(.KARA), Token(.GTEQUAL)].contains(token)
    }
    /// 配列・辞書・列挙の要素を解析する（要素の終わりまで）
    /// 形式： 要素が(は)、〜。、要素が(は)、【〜】
    /// - Parameters: token: 配列・辞書・列挙のトークン
    /// - Returns: 解析した要素の配列
    func parseElements<T>(of token: Token) -> [T]? {
        skipNextEols()
        _ = getNext(whenNextIs: ExpressionStatement.yousoga + ExpressionStatement.yousowa, matchAll: false) // 要素が、(要素は、)
        skipNextEol()
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .PERIOD
        skipNextEol()
        var elements: [T] = []
        if isEndOfElements {return elements}    // 空の配列
        repeat {
            getNext()
            skipEols()
            if currentToken == .symbol(endSymbol) || isEndOfBlock {break}   // 要素の終わりが「、」
            guard let parsed: T = parseElement(of: token) else {
                error(message: "\(token.literal)で、要素の解釈に失敗した。")
                return nil
            }
            elements.append(parsed)
        } while !nextToken.isEof && getNext(whenNextIs: .COMMA)
        if !isEndOfBlock {_ = getNext(whenNextIs: endSymbol)}
        return elements
    }
    private func parseElement<T>(of token: Token) -> T? {
        switch token {
        case .keyword(.ARRAY):
            return parseExpressionStatement() as? T
        case .keyword(.DICTIONARY):
            return parsePairExpression() as? T
        case .keyword(.ENUM):
            return parseStatement() as? T
        default:
            return nil
        }
    }
    /// 配列の要素を解析する。
    /// - Returns: 要素を式文として返す。
    private func parseExpressionStatement() -> ExpressionStatement? {
        var expressions: [Expression] = []
        let token = currentToken
        while true {
            skipEols()
            guard let expression = ExpressionPareser(parser).parse() else {
                error(message: "配列で、式の解析に失敗した。")
                return nil
            }
            expressions.append(expression)
            if isEndOfElements {break}
            getNext()
        }
        if let phrase = expressions.last as? PhraseExpression, phrase.token.literal == ExpressionStatement.to {
            expressions[expressions.count-1] = phrase.left  // 「と」を取り除く
        }
        return ExpressionStatement(token: token, expressions: expressions)
    }
    /// 列挙の要素を解析する。
    /// - Returns: 要素を文として返す。
    private func parseStatement() -> Statement? {
        var expressions: [Expression] = []
        if currentToken.isIdent && nextToken.literal == DefineStatement.wa {    // 値あり
            let ident = Identifier(token: currentToken, value: currentToken.literal)
            getNext()
            let defineToken = currentToken
            _ = getNext(whenNextIs: .COMMA)
            let valueToken = nextToken
            repeat {
                getNext()
                guard let expression = ExpressionPareser(parser).parse() else {
                    error(message: "列挙で、値の式の解析に失敗した。")
                    return nil
                }
                expressions.append(expression)
            } while !isEndOfElements
            if let phrase = expressions.last as? PhraseExpression, phrase.token.literal == ExpressionStatement.to {
                expressions[expressions.count-1] = phrase.left  // 「と」を取り除く
            }
            return DefineStatement(token: defineToken, name: ident, value: ExpressionStatement(token: valueToken, expressions: expressions))
        }
        let token = currentToken
        guard var expression = ExpressionPareser(parser).parse() else {
            error(message: "列挙で、識別子の解析に失敗した。")
            return nil
        }
        if let phrase = expression as? PhraseExpression, let ident = phrase.left as? Identifier {
            expression = ident                                  // 「と」を取り除く
        }
        return ExpressionStatement(token: token, expressions: [expression])
    }
    /// 辞書の要素を解析する。
    /// - Returns: 要素を式文のペア(索引と値)として返す。
    private func parsePairExpression() -> PairExpression? {
        var expressions: [Expression] = []
        var beginOfValueExpressions = 0     // 値の開始位置
        while true {
            skipEols()
            guard let expression = ExpressionPareser(parser).parse() else {
                error(message: "辞書で、式の解析に失敗した。")
                return nil
            }
            expressions.append(expression)
            if let phrase = expression as? PhraseExpression, phrase.token.literal == ExpressionStatement.ga {   // 区切り「が」の検出
                beginOfValueExpressions = expressions.count
                _ = getNext(whenNextIs: .COMMA) // 読点(、)を読み飛ばす
            }
            if isEndOfElements {break}
            getNext()
        }
        guard beginOfValueExpressions > 0 else {
            error(message: "辞書で、索引と値の区切り「が」が見つからなかった。")
            return nil
        }
        // 索引
        if let phrase = expressions[beginOfValueExpressions-1] as? PhraseExpression {
            expressions[beginOfValueExpressions-1] = phrase.left // 「が」を取り除く
        }
        let keyToken = Token(word: expressions[0].tokenLiteral)
        let keyExpressions = ExpressionStatement(token: keyToken, expressions: Array(expressions[0..<beginOfValueExpressions]))
        // 値
        if let phrase = expressions.last as? PhraseExpression, phrase.token.literal == ExpressionStatement.to {
            expressions[expressions.count-1] = phrase.left       // 「と」を取り除く
        }
        let valueToken = Token(word: expressions[beginOfValueExpressions].tokenLiteral)
        let valueExpressions = ExpressionStatement(token: valueToken, expressions: Array(expressions[beginOfValueExpressions..<expressions.count]))
        //
        return PairExpression(pair: (key: keyExpressions, value: valueExpressions))
    }
    private func skipNextEol() {_ = getNext(whenNextIs: .EOL)}
}
// MARK: - statemt parsers and those instance factory
struct StatementParserFactory {
    static func create(from parser: Parser) -> StatementParsable {
        if parser.currentToken.isIdent {
            switch parser.nextToken.literal {
            case DefineStatement.wa, DefineStatement.towa:
                return DefStatementParser(parser)
            default:
                break
            }
        }
        return ExpressionStatementParser(parser)
    }
}
/// - 形式１：<識別子>とは、<式(値)>ことである。
/// - 形式２：<識別子>は、<式(値)>。
struct DefStatementParser : StatementParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    let syntax1 = "定義文「<識別子>とは、<式(値)>ことである。」"
    let syntax2 = "定義文「<識別子>は、<式(値)>。」"
    func parse() -> Statement? {
        var isExtended = false
        let identifier = Identifier(token: currentToken, value: currentToken.literal) // 識別子名はそのまま
        getNext()
        let token = currentToken           // 「は」「とは」
        let syntax = (token.literal == DefineStatement.towa) ? syntax1 : syntax2
        _ = getNext(whenNextIs: .COMMA)    // 読点(、)を読み飛ばす
        if getNext(whenNextIs: DefineStatement.further) {   // 「さらに、」
            isExtended = true
            _ = getNext(whenNextIs: .COMMA) // 読点(、)を読み飛ばす
        }
        getNext()
        guard let parsed = ExpressionStatementParser(parser).parse() as? ExpressionStatement else {
            error(message: "\(syntax)で、式の解釈に失敗した。")
            return nil
        }
        _ = getNext(whenNextIs: DefineStatement.koto)
        if !isEndOfBlock {
            _ = getNext(whenNextIs: DefineStatement.dearu + DefineStatement.desu, matchAll: false)
            _ = getNext(whenNextIs: .PERIOD)
            while getNext(whenNextIs: .EOL) {}  // EOLの前で解析を停止する。
        }
        return DefineStatement(token: token, name: identifier, value: parsed, isExtended: isExtended)
    }
}
/// 文の終わりまで、式を解析する。
/// 文の終わり：句点、または改行
/// 解析停止：EOF、ブロックの終わり(】)
struct ExpressionStatementParser : StatementParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Statement? {
        var expressions: [Expression] = []
        let token = currentToken
        while !isEndOfStatement && !currentToken.isEof {
            skipEols()
            guard let expression = ExpressionPareser(parser).parse() else {
                error(message: "式文で、式の解析に失敗した。")
                return nil
            }
            expressions.append(expression)
            if getNextWhenNextIsEndOfStatement || isBreakFactor {break}    // 文の終わり
            _ = getNext(whenNextIs: .COMMA)     // 読点を読み飛ばし、
            getNext()                           // 次の式解析に
        }
        return ExpressionStatement(token: token, expressions: expressions)
    }
    private var isEndOfStatement: Bool {currentToken.isPeriod || currentToken.isEol || isEndOfBlock}
    /// 文の終わりを検出する。(例えば、「】。」の場合、isBreakFactorによるbreakを抑止する)
    private var getNextWhenNextIsEndOfStatement: Bool {
        getNext(whenNextIs: .PERIOD) ||
        getNext(whenNextIs: .RBBRACKET) ||
        getNext(whenNextIs: .EOL)
    }
}
/// ブロック(【】)内で式を解析したstatementを、statementsに格納
/// ブロッックの終わり: endSymbolを.EOLとすることで、【】を省略し行末までをブロックとして扱うことができる。
/// 解析停止：EOF、ブロックの終わり
struct BlockStatementParser : StatementParsable {
    init(_ parser: Parser, symbol: Token.Symbol = .RBBRACKET) {self.parser = parser; self.endBlockSymbol = symbol}
    let parser: Parser, endBlockSymbol: Token.Symbol
    func parse() -> Statement? {blockStatement}
    var blockStatement: BlockStatement? {
        var blockStatements: [Statement] = []
        countUp(endBlockSymbol)
        let token = currentToken
        getNext()
        while !isEndOfBlock && !currentToken.isEof {
            skipEolInBlock()                    // ブロック内での改行は読み飛ばす
            guard let statement = StatementParserFactory.create(from: parser).parse() else {
                error(message: "ブロック(【】)内で文の解析に失敗した。")
                return nil
            }
            blockStatements.append(statement)
            if (counter(.EOL) > 1 && nextToken == .symbol(.EOL)) ||
                isEndOfBlock || currentToken.isEof {break}   // 文で、】を検出（句点が検出できなかった。）
            if currentToken == .symbol(.RBBRACKET) && endBlockSymbol == .EOL {
                error(message: "ブロック(【】)の「【」と「】」が矛盾(過不足)している。")
                return nil
            }
            getNext()                                       // 句点等を読み飛ばす。
        }
        countDown(endBlockSymbol)
        return BlockStatement(token: token, statements: blockStatements)
    }
    private var isEndOfBlock: Bool {currentToken == .symbol(endBlockSymbol)}
    /// 【】ブロック内の改行は読み飛ばす。
    private func skipEolInBlock() {
        while endBlockSymbol != .EOL && currentToken == .symbol(.EOL) {getNext()}
    }
}
// MARK: - expression parser
/// 中間置演算子の優先順位(未使用)
enum Precedence : Int {
    case lowest = 1, or, and, block
    static func < (lhs: Self, rhs: Self) -> Bool {lhs.rawValue < rhs.rawValue}
    static let precedences: [Token.TokenType: Self] = [
        .keyword(.OR):          .or,
        .keyword(.AND):         .and,
    ]
    static subscript(tokenType: Token.TokenType) -> Self {precedences[tokenType] ?? .lowest}
}
struct ExpressionPareser : ExpressionParsable {
    init(_ parser: Parser, precedence: Precedence = .lowest) {self.parser = parser; self.precedence = precedence}
    let parser: Parser, precedence: Precedence
    func parse() -> Expression? {
        guard let prefix = PrefixExpressionParserFactory.create(from: parser) else {
            error(message: "式の解析で、「\(currentToken)」に対応する構文解析方法が実装されていない。")
            return nil
        }
        var leftExpression = prefix.parse()
        while !nextToken.isPeriod && precedence < nextPrecedence {
            if leftExpression is LogicalExpression {break}  // 論理式の途中で、中間置の「または」を除外
            guard let infix = InfixExpressionParserFactory.create(from: parser, with: leftExpression) else {
                return leftExpression
            }
            getNext()
            leftExpression = infix.parse()
        }
        if let postfix = PostfixExpressionParserFactory.create(from: parser, with: leftExpression) {
            getNext()
            leftExpression = postfix.parse()
        }
        return leftExpression
    }
    private var nextPrecedence: Precedence {Precedence[nextToken.type]}
}
// MARK: - prefix expression parsers and those instance factory
struct PrefixExpressionParserFactory {
    static func create(from parser: Parser) -> ExpressionParsable? {
        switch parser.currentToken.type {   // currentTokenに続くトークンを解析する解析器
        case .ident:                return IdentifierParser(parser)
        case .int:                  return IntegerLiteralParser(parser)
        case .string:               return StringLiteralParser(parser)
        case .keyword(.TRUE),.keyword(.FALSE):
                                    return BooleanParser(parser)
        case .keyword(.OR),.keyword(.AND):
                                    return LogicalExpressionParser(parser)
        case .keyword(.FUNCTION):   return FunctionLiteralParser(parser)
        case .keyword(.TYPE):       return TypeLiteralParser(parser)
        case .keyword(.ARRAY):      return ArrayLiteralParser(parser)
        case .keyword(.DICTIONARY): return DictionaryLiteralParser(parser)
        case .keyword(.ENUM):       return EnumLiteralParser(parser)
        case .keyword(.RANGE):      return RangeLiteralParser(parser)
        case .keyword(.CASE):       return CaseExpressionParser(parser)
        case .keyword(.LOOP):       return LoopExpressionParser(parser)
        case .keyword(.IDENTIFIER),.keyword(.FILE),.keyword(.POSITION):
                                    return LabelExpressionParser(parser)
        case .keyword(_):           return PredicateExpressionParser(parser)
        default:                    return nil
        }
    }
}
struct IdentifierParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {parseRangeExpression(with: Identifier(from: currentToken))}
}
struct StringLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {StringLiteral(from: currentToken)}
}
struct IntegerLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        guard let value = currentToken.number else {
            error(message: "整数リテラルの解析で、「\(currentToken.literal)」を整数に変換できなかった。")
            return nil
        }
        return parseRangeExpression(with: IntegerLiteral(from: value))
    }
}
struct BooleanParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {Boolean(from: currentToken.isTrue)}
}
/// ラベル(キーワード)と、後続の文字列(または識別子)を記憶する。
/// ※：例：ファイル「text.txt」、識別子『割った余り』
/// ※：キーワードに識別子を続けると、合成された識別子となるので、『』で明示的に表記する必要がある。
struct LabelExpressionParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = currentToken           // Token.Keyword
        guard nextToken.type == .string || nextToken.type == .ident || nextToken.type == .int else {
            error(message: "「\(token.literal)」の後続が「文字列」(または「識別子」)でなかった。)")
            return nil
        }
        getNext()
        return Label(token: token, value: currentToken.literal)
    }
}
// 1. 範囲【<範囲式><キーワード>】
// 2. 範囲【<下限式><下限キーワード><上限式><上限キーワード>】
// <下限キーワード>: 以上、から
// <上限キーワード>: 以下、未満、まで
// ※：範囲式内の上下限キーワード使用はエラー(例：範囲【１０から１を引くから、１０に１を足すまで】)
struct RangeLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = currentToken            // 範囲
        guard getNext(whenNextIs: .LBBRACKET) else {
            error(message: "範囲は、ブロック記号「【】」が必要。")
            return nil
        }
        guard let block = BlockStatementParser(parser, symbol: .RBBRACKET).blockStatement else {
            error(message: "範囲で、範囲式の解析に失敗した。")
            return nil
        }
        guard block.statements.count == 1,
              let es = block.statements.first as? ExpressionStatement else {
            error(message: "範囲で、範囲式の解析に失敗した。(式が取り出せない。)")
            return nil
        }
        return parseRangeExpressions(es.expressions, token: token)
    }
}
struct FunctionLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = currentToken            // 関数であって、(であり、)
        _ = getNext(whenNextIs: ExpressionStatement.deatte + ExpressionStatement.deari, matchAll: false)
        // Prameter block 解析
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL
        guard let paramenters = parseParameters(endSymbol: endSymbol) else {
            error(message: "関数で、「入力が〜であり、」の解析に失敗した。")
            return nil
        }
        let identifiers = paramenters.map {$0.0}
        let signature = parseSignature(from: paramenters.map {$0.1})
        while nextToken.isEol {getNext()}   // 【とEOLを飛ばす
        // Body block 解析
        _ = getNext(whenNextIs: ExpressionStatement.hontaiga + ExpressionStatement.hontaiwa, matchAll: false)   // 本体が、(本体は、)
        guard let body = BlockStatementParser(parser, symbol: endSymbol).blockStatement else {
            error(message: "関数で、「本体が、〜」の解析に失敗した。")
            return nil
        }
        return FunctionLiteral(token: token, parameters: identifiers, signature: signature, body: body)
    }
}
struct TypeLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        if parser.previousToken == .particle(.NO) {return Identifier(from: currentToken)}   // 〜の型：識別子として振る舞う
        let token = currentToken            // 型であって、(であり、)
        _ = getNext(whenNextIs: ExpressionStatement.deatte + ExpressionStatement.deari, matchAll: false)
        // Prameter block 解析
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL
        guard let paramenters = parseParameters(endSymbol: endSymbol) else {
            error(message: "型で、「入力が〜であり、」の解析に失敗した。")
            return nil
        }
        let identifiers = paramenters.map {$0.0}
        let signature = parseSignature(from: paramenters.map {$0.1})
        while nextToken.isEol {getNext()}   // 【とEOLを飛ばす
        // Initialyzer block 解析
        var initialyzer: BlockStatement?
        if getNext(whenNextIs: ExpressionStatement.syokika) {   // 初期化は、(初期化が、)
            _ = getNext(whenNextIs: ExpressionStatement.wa + ExpressionStatement.ga, matchAll: false)
            _ = getNext(whenNextIs: .COMMA)
            let endOfInit: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL
            guard let block = BlockStatementParser(parser, symbol: endOfInit).blockStatement else {
                error(message: "型で、「初期化は、〜」の解析に失敗した。")
                return nil
            }
            initialyzer = block
            _ = getNext(whenNextIs: .PERIOD)                    // 初期化ブロックの句点を飛ばす
        }
        // Body block 解析
        _ = getNext(whenNextIs: ExpressionStatement.hontaiga + ExpressionStatement.hontaiwa, matchAll: false)   // 本体が、(本体は、)
        guard let body = BlockStatementParser(parser, symbol: endSymbol).blockStatement else {
            error(message: "型で、「本体が、〜」の解析に失敗した。")
            return nil
        }
        return TypeLiteral(token: token, parameters: identifiers, signature: signature, initializer: initialyzer, body: body)
    }
}
struct EnumLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = currentToken            // 列挙であって、(であり、)
        _ = getNext(whenNextIs: ExpressionStatement.deatte + ExpressionStatement.deari, matchAll: false)
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .PERIOD
        // 要素の解析
        guard let elements: [Statement] = parseElements(of: token) else {
            error(message: "列挙で、「要素が、〜」の解析に失敗した。")
            return nil
        }
        skipNextEols()
        guard currentToken == .symbol(endSymbol) || getNext(whenNextIs: endSymbol, withError: true) else {return nil}
        return EnumLiteral(token: token, elements: elements)
    }
}
struct ArrayLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = currentToken                                // 配列であって、(であり、)
        _ = getNext(whenNextIs: ExpressionStatement.deatte + ExpressionStatement.deari, matchAll: false)
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .PERIOD
        // 要素の解析
        guard let elements: [ExpressionStatement] = parseElements(of: token) else {
            error(message: "配列で、「要素が、〜」の解析に失敗した。")
            return nil
        }
        skipNextEols()
        guard currentToken == .symbol(endSymbol) || getNext(whenNextIs: endSymbol, withError: true) else {return nil}
        return ArrayLiteral(token: token, elements: elements)
    }
}
struct DictionaryLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = currentToken            // 辞書であって、(であり、)
        _ = getNext(whenNextIs: ExpressionStatement.deatte + ExpressionStatement.deari, matchAll: false)
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .PERIOD
        // 要素の解析
        guard let pairs: [PairExpression] = parseElements(of: token) else {
            error(message: "辞書で、「要素が、〜」の解析に失敗した。")
            return nil
        }
        skipNextEols()
        guard currentToken == .symbol(endSymbol) || getNext(whenNextIs: endSymbol, withError: true) else {return nil}
        return DictionaryLiteral(token: token, pairs: pairs)
    }
}
struct PredicateExpressionParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {PredicateExpression(token: currentToken)}
}
/// 「場合」で始まる式を解析し、「場合、」に続くブロックをCaseExpression.consequenceとし、
/// 続いて「それ以外は、」があれば、それに続くブロックをCaseExpression.alternativeとする。
struct CaseExpressionParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = currentToken                            // 場合
        _ = getNext(whenNextIs: .COMMA)                     // (、)
        var endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL
        guard let consequence = BlockStatementParser(parser, symbol: endSymbol).blockStatement else {
            error(message: "「場合、」に続くブロック解析に失敗した。")
            return nil
        }
        _ = getNext(whenNextIs: .COMMA)                     // (、)のみ読み飛ばす
        var alternative: BlockStatement? = nil
        if getNext(whenNextIs: ExpressionStatement.soreigai) {// それ以外
            _ = getNext(whenNextIs: ExpressionStatement.wa) // (は)
            _ = getNext(whenNextIs: .COMMA)                 // (、)
            endSymbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL
            alternative = BlockStatementParser(parser, symbol: endSymbol).blockStatement
            if alternative == nil {
                error(message: "場合文の「それ以外、」に続くブロック解析に失敗した。")
                return nil
            }
        }
        return CaseExpression(token: token, consequence: consequence, alternative: alternative)
    }
}
/// 「または」または「かつ」で始まる式を解析し、に続くブロック(または条件式)をLogicalExpression.rightとする。
struct LogicalExpressionParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = currentToken                            // 「または」または「かつ」
        _ = getNext(whenNextIs: .COMMA)                     // (、)
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL
        guard let consequence = parseLogicalExpressions(parser, symbol: endSymbol) else {
            error(message: "「\(currentToken.literal)、」に続くブロック解析に失敗した。")
            return nil
        }
        _ = getNext(whenNextIs: .COMMA)                     // (、)のみ読み飛ばす
        return LogicalExpression(token: token, right: consequence)
    }
    ///  論理式の右辺(条件式を解析する)
    /// - Parameters:
    ///   - parser: 解析器
    ///   - symbol: ブロックの終端。「】」ならばブロック、そうでなければ条件式として解析する。
    /// - Returns: ブロック文を返す。(解析失敗は、nil)
    private func parseLogicalExpressions(_ parser: Parser, symbol: Token.Symbol) -> BlockStatement? {
        if symbol == .RBBRACKET {
            return BlockStatementParser(parser, symbol: symbol).blockStatement
        }
        getNext()
        var expressions: [Expression] = []
        let token = currentToken
        while !isEndOfStatement && !isEndOfBlock && !currentToken.isEof {
            skipEols()
            guard let expression = ExpressionPareser(parser).parse() else {
                error(message: "条件式で、右辺の解析に失敗した。")
                return nil
            }
            expressions.append(expression)
            if isEndOfLogicalExpression(expression) || isBreakFactor ||
                getNext(whenNextIs: .PERIOD) || getNext(whenNextIs: .EOL) {
                break
            }   // 文の終わり、または停止要因
            _ = getNext(whenNextIs: .COMMA)     // 読点を読み飛ばし、
            getNext()                           // 次の式解析に
        }
        let statement = ExpressionStatement(token: token, expressions: expressions)
        return BlockStatement(token: token, statements: [statement])
    }
    /// 条件式の終わりを、述語(PredicateExpression)として判断する。
    /// - Parameter expression: 式
    /// - Returns: 条件式か否か
    private func isEndOfLogicalExpression(_ expression: Expression) -> Bool {
        expression is PredicateExpression
    }
}
///「反復」で始まる式を解析する。
/// 1. <数値>から<数値>まで(未満)(<数値>ずつ)反復【入力が<(カウンターの)識別子>、<処理>】
/// 2. 反復【条件が<条件式>の間、<処理>】
/// 3. 反復【<処理>】（処理を中止するには、「中止(する)」を使用する)
/// 4. <配列、辞書>を反復【入力が<(要素の）識別子>、<処理>】
struct LoopExpressionParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = currentToken                            // 反復
        _ = getNext(whenNextIs: .COMMA)                     // (、)
        // Prameter block 解析
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL
        guard let paramenters = parseParameters(endSymbol: endSymbol) else {
            error(message: "反復で、「入力が〜であり、」の解析に失敗した。")
            return nil
        }
        let identifiers = paramenters.map {$0.0}
        // Condition block 解析
        guard let condition = parseCondition(endSymbol: endSymbol) else {
            error(message: "反復で、「条件が〜の間、」の解析に失敗した。")
            return nil
        }
        // Body block 解析
        _ = getNext(whenNextIs: ExpressionStatement.syoriga + ExpressionStatement.syoriwa, matchAll: false)   // 処理が、(処理は、)
        guard let body = BlockStatementParser(parser, symbol: endSymbol).blockStatement else {
            error(message: "反復で、処理の解析に失敗した。")
            return nil
        }
        return LoopExpression(token: token, parameters: identifiers, condition: condition, body: body)
    }
    private func parseCondition(endSymbol: Token.Symbol) -> [Expression]? {
        guard getNext(whenNextIs: ExpressionStatement.condition) else {return []}  // 空のパラメータ
        _ = getNext(whenNextIs: ExpressionStatement.ga + ExpressionStatement.wa, matchAll: false)   // 条件が、(条件は、)
        _ = getNext(whenNextIs: .COMMA)
        getNext()
        var expressions: [Expression] = []
        while true {
            skipEols()
            guard let expression = ExpressionPareser(parser).parse() else {
                error(message: "反復で、条件式の解析に失敗した。")
                return nil
            }
            expressions.append(expression)
            _ = getNext(whenNextIs: ExpressionStatement.aida)
            if isEndOfElements {break}
            getNext()
        }
        _ = getNext(whenNextIs: .COMMA)
        return expressions
    }
}
// MARK: - infix expression parsers and those instance factory
struct InfixExpressionParserFactory {
    static func create(from parser: Parser, with left: Expression?) -> ExpressionParsable? {
        switch parser.nextToken.type {   // nextTokenに続くトークンを解析する解析器
        case .keyword(.OR):         return InfixExpressionParser(parser, with: left)
        default:                    return nil
        }
    }
}
struct InfixExpressionParser : ExpressionParsable {
    init(_ parser: Parser, with left: Expression?) {self.parser = parser; self.left = left}
    let parser: Parser
    let left: Expression?
    func parse() -> Expression? {
        let token = currentToken
        let op = currentToken.literal
        guard let left = left else {
            error(message: "中間置式(\(op)で、左辺の解析に失敗した。")
            return nil
        }
        _ = getNext(whenNextIs: .COMMA)
        getNext()
        let precedence = Precedence[currentToken.type]
        guard let right = ExpressionPareser(parser, precedence: precedence).parse() else {
            error(message: "中間置式(\(op)で、右辺の解析に失敗した。")
            return nil
        }
        return InfixExpression(token: token, left: left, right: right)
    }
}
// MARK: - postfix expression parsers and those instance factory
struct PostfixExpressionParserFactory {
    static func create(from parser: Parser, with left: Expression?) -> ExpressionParsable? {
        switch parser.nextToken.type {   // nextTokenに続くトークンを解析する解析器
        case .particle(_):        return PhraseExpressionParser(parser, with: left)
        default:                  return nil
        }
    }
}
struct PhraseExpressionParser : ExpressionParsable {
    init(_ parser: Parser, with left: Expression?) {self.parser = parser; self.left = left}
    let parser: Parser
    let left: Expression?
    func parse() -> Expression? {
        let token = currentToken
        if token == .particle(.KO) && nextToken.isParticle {getNext()}
        guard let left = left else {
            error(message: "「\(token.literal)」格の左辺(式)の解析に失敗した。")
            return nil
        }
        return PhraseExpression(token: token, left: left)
    }
}
