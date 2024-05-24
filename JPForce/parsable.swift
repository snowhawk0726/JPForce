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
    var previousToken: Token{parser.previousToken}
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
    func skipNextEols(suppress: Bool = false) {while !suppress && nextToken.isEol {getNext()}}
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
    func error(message: String) {
        parser.errors.append(message + (currentToken.isIllegal ? "" : "(解析位置: \(currentToken.literal))"))
    }
    /// ブロックカウンター制御
    ///  - ブロック文中のブロック記号【】とEOLをカウントし、整合性をチェックする。
    var blockCount: Parser.NestCounter {parser.nestedBlockCounter}
    ///  - 要素を持つ型でのブロック記号【】と読点「。」をカウントし、整合性をチェックする。
    var elementsCount: Parser.NestCounter {parser.nestedElementsCounter}
    /// 入力部(Input Block)の解析
    /// - 形式1: 入力が、識別子1(「型格」)(は<既定値>)と 識別子2(「型格」)(は<既定値>)と...、であり、
    /// - 形式2: 入力が、識別子1(「型格」)(は<既定値>)と 識別子2(「型格」)(は<既定値>)と...識別子n（「型格」）(は<既定値>)。
    func parseParameters() -> [(Identifier, String, ExpressionStatement?)]? {
        guard isInputBlock else {return []}                     // 入力部無し
        var parameters: [(Identifier, String, ExpressionStatement?)] = []
        var foundTerminator = false                             // 既定値式中に終端文字があったか
        repeat {
            getNext()
            let identifier = Identifier(from: currentToken)     // 識別子
            let format = getNext(whenNextIs: .string) ? 
                            currentToken.literal : ""           // 入力形式
            var value: ExpressionStatement? = nil
            if getNext(whenNextIs: .WA) {                       // 既定値あり
                _ = getNext(whenNextIs: .COMMA)
                getNext()
                let token = currentToken
                guard var expressions = parseElement(until: .TO) else {
                    error(message: "入力部で、既定値の式の解析に失敗した。")
                    return nil
                }
                foundTerminator = removeTerminators(from: &expressions)
                value = ExpressionStatement(token: token, expressions: expressions)
            }
            parameters.append((identifier, format, value))
            _ = getNext(whenNextIs: .TO)                        // と
            _ = getNext(whenNextIs: .COMMA)
            guard !nextToken.isEof else {return nil}
        } while !(isEndOfParameter || foundTerminator)
        _ = getNext(whenNextIs: FunctionBlock.ari)              // (あり)
        _ = getNext(whenNextIs: .COMMA)                         // (、)
        return parameters
    }
    private var isEndOfParameter: Bool {
        getNext(whenNextIs: .DE) ||
        getNext(whenNextIs: .PERIOD) || nextToken == .symbol(.RBBRACKET)
    }
    private var isInputBlock: Bool {
        guard getNext(whenNextIs: FunctionBlock.input) else {return false}
        _ = getNext(whenNextIs: ExpressionStatement.ga + ExpressionStatement.wa, matchAll: false)   // 入力が、(入力は、)
        _ = getNext(whenNextIs: .COMMA)
        return true
    }
    private func removeTerminators(from expressions: inout [Expression]) -> Bool {
        _ = remove(lastParticle: .TO, from: &expressions)       //「と」を除外
        if expressions.last?.tokenLiteral == FunctionBlock.ari {
            expressions.removeLast()                            //「あり」を除外
        }
        return remove(lastParticle: .DE, from: &expressions)    //「で」を検出、除外
    }
    func parseSignature(from strings: [String], _ values: [ExpressionStatement?]) -> InputFormat {
        let formats = strings.map { string in
            var type = "", particle = ""
            let lexer = Lexer(string)
            var token = lexer.getNext()
            if token.isIdent || token.isKeyword {type = token.literal;token = lexer.getNext()}
            if token.isParticle {particle = token.literal;token = lexer.getNext()}
            if token.literal.isThreeDots {particle += token.literal}
            return (type, particle)
        }
        let number = formats.map({$0.1}).contains {$0.hasThreeDots} ? nil : strings.count
        return InputFormat(numberOfInputs: number, formats: formats, values: values)
    }
    func parseProtocols() -> [String]? {
        var protocols: [String] = []
        skipNextEols()
        _ = getNext(whenNextIs: ProtocolLiteral.junkyosuru)     // 準拠する
        if getNext(whenNextIs: ProtocolLiteral.kiyaku) {        // 規約は、(規約が、)
            _ = getNext(whenNextIs: ExpressionStatement.wa + ExpressionStatement.ga, matchAll: false)
            _ = getNext(whenNextIs: .COMMA)
            repeat {
                guard nextToken.isIdent || nextToken.isString else {
                    error(message: "型で、準拠する規約の型が間違っている。(\(nextToken))")
                    return nil
                }
                protocols.append(nextToken.literal)
                getNext()
                _ = getNext(whenNextIs: .TO)
            } while getNext(whenNextIs: .COMMA)
            _ = getNext(whenNextIs: .PERIOD)
        }
        return protocols
    }
    /// 式の解析　(例：1以上→範囲【1以上】)
    /// - Parameter expression: 式(数値、識別子)
    /// - Returns: 上限もしくは下限の範囲リテラル(もしくは元の式)
    func parseRangeExpression(with expression: Expression) -> Expression? {
        let token = nextToken
        switch token {
        case Token(.GTEQUAL):
            getNext()
            return RangeLiteral(token: .keyword(.RANGE), lowerBound: ExpressionStatement(token: token, expressions: [expression]))
        case Token(.LTEQUAL),Token(.UNDER):
            getNext()
            return RangeLiteral(token: .keyword(.RANGE), upperBound: ExpressionStatement(token: token, expressions: [expression]))
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
        } else {
            guard rest.isEmpty else {
                error(message: "範囲で、範囲式の解析に失敗した。(上限の形式が間違っている。)")
                return nil
            }
        }
        return RangeLiteral(token: token, lowerBound: lowerBound, upperBound: upperBound)
    }
    /// 解析された式の配列から、上限式もしくは下限式を抽出する。
    /// *1* 「<式><キーワード>」は、範囲【<範囲式>】(RangeLiteral)と解析されている。(範囲式は、以上、以下、未満を含む式)
    /// *2* 「<式>から」または「<式>まで」は、句(PhraseExpression)に解析されている。
    /// *3* キーワードを含む「<複数式>」(解析未完了)
    /// - Parameters:
    ///   - tokens: 上下限のトークン
    ///   - expressions: 式の配列
    /// - Returns: 抽出した上限式もしくは下限式(式文)
    private func getBound(of tokens: [Token], from expressions: [Expression]) -> ExpressionStatement? {
        // 式がIntegerLiteralで、tokenが、RangeLiteralまたはPhraseExpressionに解析済みの場合
        if let e = expressions.first as? RangeLiteral {return isLowBound(tokens.first) ? e.lowerBound : e.upperBound}
        if let e = expressions.first as? PhraseExpression, tokens.contains(e.token) {return ExpressionStatement(token: e.token, expressions: [e.left])}
        // 複数式から、キーワードを拾いだす。(index.0: 下限キーワード、index.1: 拾いだした式の位置)
        guard let index = firstIndex(of: expressions, by: tokens) else {return nil}
        var rangeExpressions = [Expression](expressions[0..<index.1])
        if let p = expressions[index.1] as? PhraseExpression {rangeExpressions.append(p.left)}  // キーワード(格)を除いた式を追加
        return ExpressionStatement(token: index.0, expressions: rangeExpressions)
    }
    /// expressionsから、es部分を除いた残りを返す。
    /// - Parameters:
    ///   - expressions: 入力の式配列
    ///   - es: 下限部
    /// - Returns: 残りの式配列
    private func getRest(of expressions: [Expression], except es: ExpressionStatement?) -> [Expression] {
        guard let es = es else {return expressions}
        let position = es.expressions.count
        guard position < expressions.count else {return []}
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
                if let p = $0 as? PhraseExpression {return p.token == t}
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
    func parseElements<T>(of token: Token, with endSymbol: Token.Symbol) -> [T]? {
        skipNextEols()
        _ = getNext(whenNextIs: ExpressionStatement.yousoga + ExpressionStatement.yousowa, matchAll: false) // 要素が、(要素は、)
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
            return parseArrayElement() as? T
        case .keyword(.DICTIONARY):
            return parseDictionaryElement() as? T
        case .keyword(.ENUM):
            return parseEnumElement() as? T
        default:
            return nil
        }
    }
    /// 配列の要素を解析する。
    /// - Returns: 要素を式文として返す。
    private func parseArrayElement() -> ExpressionStatement? {
        let token = currentToken
        guard var expressions = parseElement() else {
            error(message: "配列で、式の解析に失敗した。")
            return nil
        }
        _ = remove(lastParticle: .TO, from: &expressions)   // 「と」を取り除く
        return ExpressionStatement(token: token, expressions: expressions)
    }
    /// 列挙の要素を解析する。
    /// - Returns: 要素を文(定義文または式文)として返す。
    private func parseEnumElement() -> Statement? {
        let token = currentToken
        guard token.isIdent else {
            error(message: "列挙で、識別子の解析に失敗した。")
            return nil
        }
        let ident = Identifier(from: token)
        if getNext(whenNextIs: .WA) {  // 値あり
            let defineToken = currentToken
            _ = getNext(whenNextIs: .COMMA)
            getNext()
            let valueToken = currentToken
            guard var expressions = parseElement() else {
                error(message: "列挙で、値の式の解析に失敗した。")
                return nil
            }
            _ = remove(lastParticle: .TO, from: &expressions)   // 「と」を取り除く
            return DefineStatement(token: defineToken, name: ident, value: ExpressionStatement(token: valueToken, expressions: expressions))
        } else {
            _ = getNext(whenNextIs: .TO)
            return ExpressionStatement(token: token, expressions: [ident])
        }
    }
    /// 辞書の要素を解析する。
    /// - Returns: 要素を式文のペア(索引と値)として返す。
    private func parseDictionaryElement() -> PairExpression? {
        // 索引の解析
        guard var expressions = parseElement(until: .GA) else {
            error(message: "辞書で、索引の式の解析に失敗した。")
            return nil
        }
        _ = getNext(whenNextIs: .COMMA)
        guard remove(lastParticle: .GA, from: &expressions) else {  //「が」を取り除く
            error(message: "辞書で、索引と値の区切り「が」が見つからなかった。")
            return nil
        }
        let keyToken = Token(word: expressions[0].tokenLiteral)
        let keyExpressions = ExpressionStatement(token: keyToken, expressions: expressions)
        getNext()
        // 値の解析
        guard var expressions = parseElement() else {
            error(message: "辞書で、値の式の解析に失敗した。")
            return nil
        }
        _ = remove(lastParticle: .TO, from: &expressions)           //「と」を取り除く
        let valueToken = Token(word: expressions[0].tokenLiteral)
        let valueExpressions = ExpressionStatement(token: valueToken, expressions: expressions)
        //
        return PairExpression(pair: (key: keyExpressions, value: valueExpressions))
    }
    private func skipNextEol() {_ = getNext(whenNextIs: .EOL)}
    /// 要素式の最後尾(句)から、格を取り除く
    /// - Parameters:
    ///   - p: 取り除く格
    ///   - expressions: 要素式
    /// - Returns: 取り除いた(true)か否(false)か
    private func remove(lastParticle p: Token.Particle, from expressions: inout [Expression]) -> Bool {
        if let phrase = expressions.last as? PhraseExpression, phrase.token.isParticle(p) {
            expressions[expressions.endIndex - 1] = phrase.left
            return true
        }
        return false
    }
    /// 要素を解析する
    /// - Parameter p: 終端の格、または無し(nil)
    /// - Returns: 解析した式の配列、nil: 解析失敗
    private func parseElement(until p: Token.Particle? = nil) -> [Expression]? {
        var expressions: [Expression] = []
        while true {
            skipEols()
            guard let expression = ExpressionParser(parser).parse() else {
                return nil
            }
            expressions.append(expression)
            if let phrase = expression as? PhraseExpression,
               let particle = p, phrase.token.isParticle(particle) {
                return expressions      // 指定格があった
            }
            if isEndOfElements {break}
            getNext()
        }
        return expressions
    }
    // MARK: - Literal Parser Common Procs
    /// ヘッダー部：　<型名>であって、(<型名>であり、)
    /// - Returns: <型>トークン
    func parseHeader() -> Token {
        let token = currentToken
        _ = getNext(whenNextIs: ExpressionStatement.deatte + ExpressionStatement.deari, matchAll: false)
        return token
    }
    /// 入力部： 入力が(は)<識別子>「<型><格>」…であり、
    /// - Returns: 識別子の配列および入力形式、もしくはエラー(nil)
    func parseParameterBlock(in type: String) -> ([Identifier], InputFormat)? {
        guard let paramenters = parseParameters() else {
            error(message: "\(type)で、「入力が〜」の解析に失敗した。")
            return nil
        }
        return (paramenters.map {$0.0}, 
                parseSignature(from: paramenters.map {$0.1}, paramenters.map {$0.2})
        )
    }
    /// 定義部： <ブロック名>は(が)、【<定義>】
    /// - Returns: ブロック文、定義無し(nil)、もしくはエラー
    func parseOptionalBlock(of blockname: String, in typename: String) -> Result<BlockStatement?, FunctionBlockError> {
        guard getNext(whenNextIs: blockname) else {return .success(nil)}
        _ = getNext(whenNextIs: ExpressionStatement.ga + ExpressionStatement.wa, matchAll: false)
        _ = getNext(whenNextIs: .COMMA)
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL
        guard let blockStatement = BlockStatementParser(parser, symbol: endSymbol).blockStatement else { 
            error(message: "\(typename)で、「\(blockname)は、〜」の解析に失敗した。")
            return .failure(FunctionBlockError.body)
        }
        _ = getNext(whenNextIs: .PERIOD)    // ブロックの句点を飛ばす
        return .success(blockStatement)
    }
    func isEndOfBlock(of symbol: Token.Symbol) -> Bool {
        nextToken == .symbol(symbol)
    }
    /// 関数部：【入力が(は)、〜。本体が、〜。】
    /// - Returns: 関数ブロック、もしくはエラー
    func parseFunctionBlock(in typename: String, endOfBlock: Token.Symbol? = nil, isOverloaded: Bool = false) -> Result<FunctionBlock, FunctionBlockError> {
        let endSymbol: Token.Symbol = (endOfBlock == nil) ?
        getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL :
        endOfBlock!
        // Prameter block
        guard let (identifiers, signature) = parseParameterBlock(in: typename) else {return .failure(FunctionBlockError.parameter)}
        var body: BlockStatement?
        if !getNext(whenNextIs: endSymbol) && nextToken != .symbol(.RBBRACKET) {
            // FunctionBlocks block
            _ = getNext(whenNextIs: ExpressionStatement.hontaiga + ExpressionStatement.hontaiwa, matchAll: false)   // 本体が、(本体は、)
            guard let block = BlockStatementParser(parser, symbol: endSymbol).blockStatement else {return .failure(FunctionBlockError.body)}
            body = block
        }
        _ = getNext(whenNextIs: .PERIOD)    // ブロックの句点を飛ばす
        return .success(FunctionBlock(
            parameters: identifiers,
            signature: signature,
            body: body,
            isOverloaded: isOverloaded
        ))
    }
    func parseFunctionBlocks(of name: String, in typename: String) -> Result<FunctionBlocks, FunctionBlockError> {
        var functionBlocks = FunctionBlocks()
        while getNext(whenNextIs: name) {
            _ = getNext(whenNextIs: ExpressionStatement.ga + ExpressionStatement.wa, matchAll: false)
            _ = getNext(whenNextIs: .COMMA)
            let isOverloaded = getNext(whenNextIs: DefineStatement.further)
            _ = getNext(whenNextIs: .COMMA)

            //
            switch parseFunctionBlock(in: typename, isOverloaded: isOverloaded) {
            case .success(let function):
                _ = functionBlocks.append(function)
            case .failure(let error):
                return .failure(error)
            }
            _ = getNext(whenNextIs: .PERIOD)    // ブロックの句点を飛ばす
            _ = getNext(whenNextIs: .EOL)       // EOLを飛ばす
        }
        return .success(functionBlocks)
    }
    func parseEndOfElementsLiteral(with endSymbol: Token.Symbol) -> Bool {
        skipNextEols()
        if currentToken != .symbol(endSymbol) {
            if endSymbol == .RBBRACKET {_ = getNext(whenNextIs: .PERIOD)}   // 】の前の「。」は読み飛ばす
            skipNextEols()
            guard getNext(whenNextIs: endSymbol, withError: true) else {return false}
        } else {                                                            // 】の時は、進めない
            if elementsCount.value(of: .RBBRACKET) < 1 {_ = getNext(whenNextIs: .RBBRACKET)}    // ただし、】】の時は、１つ進める。
        }
        return true
    }
}
enum FunctionBlockError : Error {
    case parameter, body
    var message: String {
        switch self {
        case .parameter:    return "(入力の解析エラー)"
        case .body:         return "(本体の解析エラー)"
        }
    }
}
//
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
        let identifier = Identifier(from: currentToken)
        parser.insert(identifier.value)     // 識別子をLexerに登録
        getNext()
        let token = currentToken            // 「は」「とは」
        let syntax = (token.literal == DefineStatement.towa) ? syntax1 : syntax2
        _ = getNext(whenNextIs: .COMMA)     // 読点(、)を読み飛ばす
        let isExtended = getNext(whenNextIs: DefineStatement.further)   // 「さらに、」
        _ = getNext(whenNextIs: .COMMA)
        getNext()
        guard let parsed = ExpressionStatementParser(parser).parse() as? ExpressionStatement else {
            error(message: "\(syntax)で、式の解釈に失敗した。")
            return nil
        }
        _ = getNext(whenNextIs: DefineStatement.koto)
        if !isEndOfBlock {
            _ = getNext(whenNextIs: DefineStatement.dearu + DefineStatement.desu, matchAll: false)
            _ = getNext(whenNextIs: .PERIOD)
            skipNextEols()                  // EOLの前で解析を停止する。
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
            guard let expression = ExpressionParser(parser).parse() else {
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
        blockCount.up(to: endBlockSymbol)
        getNext()
        let token = currentToken
        while !isEndOfBlock && !currentToken.isEof {
            skipEolInBlock()                    // ブロック内での改行は読み飛ばす
            guard let statement = StatementParserFactory.create(from: parser).parse() else {
                error(message: "ブロック(【】)内で文の解析に失敗した。")
                return nil
            }
            blockStatements.append(statement)
            if (blockCount.value(of: .EOL) > 1 && nextToken == .symbol(.EOL)) ||
                isEndOfBlock || currentToken.isEof {break}   // 文で、】を検出（句点が検出できなかった。）
            if currentToken == .symbol(.RBBRACKET) {break}
            getNext()                                       // 句点等を読み飛ばす。
            skipEolInBlock()
        }
        blockCount.down(to: endBlockSymbol)
        return BlockStatement(token: token, statements: blockStatements)
    }
    private var isEndOfBlock: Bool {
        currentToken == .symbol(endBlockSymbol)
    }
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
struct ExpressionParser : ExpressionParsable {
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
        case .keyword(.COMPUTATION):
                                    return ComputationLiteralParser(parser)
        case .keyword(.PROTOCOL):   return ProtocolLiteralParser(parser)
        case .keyword(.TYPE):       return TypeLiteralParser(parser)
        case .keyword(.ARRAY):      return ArrayLiteralParser(parser)
        case .keyword(.DICTIONARY): return DictionaryLiteralParser(parser)
        case .keyword(.ENUM):       return EnumLiteralParser(parser)
        case .keyword(.RANGE):      return RangeLiteralParser(parser)
        case .keyword(.CASE):       return CaseExpressionParser(parser)
        case .keyword(.LOOP):       return LoopExpressionParser(parser)
        case .keyword(.CONDITIONAL):return ConditionalOperationParser(parser)
        case .keyword(.IDENTIFIER),.keyword(.FILE),.keyword(.POSITION),.keyword(.MEMBER):
                                    return LabelExpressionParser(parser)
        case .keyword(_):           return PredicateExpressionParser(parser)
        case .illegal:              break
        default:                    return nil
        }
        print("字句解析エラー： \(parser.currentToken.literal)。")
        return nil
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
            error(message: "「\(token.literal)」の後続が<文字列>、<識別子>、または、<数値>でなかった。)")
            return nil
        }
        getNext()
        return Label(token: token, value: currentToken)
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
        let token = parseHeader()
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL
        guard let block = BlockStatementParser(parser, symbol: endSymbol).blockStatement else {
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
        let token = parseHeader()
        switch parseFunctionBlock(in: token.literal) {
        case .success(let function):
            return FunctionLiteral(token: token, functions: FunctionBlocks(function))
        case .failure(_):
            return nil
        }
    }
}
struct ComputationLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = parseHeader()
        let endOfType: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL
        skipNextEols(suppress: endOfType == .EOL)
        // Setter block
        var setters = FunctionBlocks()
        switch parseFunctionBlocks(of: ComputationLiteral.settei, in: token.literal) {
        case .success(let blocks):
            setters = blocks
            _ = getNext(whenNextIs: .PERIOD)        // 設定ブロックの句点を飛ばす
            skipNextEols(suppress: endOfType == .EOL)
        case .failure(let e):
            error(message: "算出で、「設定が、〜」の解析に失敗した。\(e.message)")
            return nil
        }
        // Getter block
        var getters = FunctionBlocks()
        if !isEndOfBlock(of: endOfType) {
            switch parseFunctionBlocks(of: ComputationLiteral.syutoku, in: token.literal) {
            case .success(let blocks):
                getters = blocks
                _ = getNext(whenNextIs: .PERIOD)    // 取得ブロックの句点を飛ばす
                skipNextEols(suppress: endOfType == .EOL)
            case .failure(let e):
                error(message: "算出で、「取得が、〜」の解析に失敗した。\(e.message)")
                return nil
            }
            if getters.isEmpty {                    // 「取得は、」が無かった
                switch parseFunctionBlock(in: token.literal, endOfBlock: endOfType) {
                case .success(let function):
                    _ = getters.append(function)
                case .failure(let e):
                    error(message: "算出の解析に失敗した。\(e.message)")
                    return nil
                }
            }
        }
        if !setters.isEmpty {_ = getNext(whenNextIs: endOfType)}    // 算出ブロックの終わりを読み飛ばす。
        _ = getNext(whenNextIs: .PERIOD)            // 算出ブロックの句点を飛ばす
        skipNextEols(suppress: endOfType == .EOL)
        return ComputationLiteral(token: token, setters: setters, getters: getters)
    }
}
struct ProtocolLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = parseHeader()
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL
        // Protocols block
        guard let protocols = parseProtocols() else {return nil}
        // Clauses block
        guard let clauses = parseClauses(until: endSymbol) else {
            error(message: "規約で、「条項が、〜」の解析に失敗した。")
            return nil
        }
        if endSymbol == .RBBRACKET {_ = getNext(whenNextIs: endSymbol)}
        return ProtocolLiteral(token: token, protocols: protocols, clauses: clauses)
    }
    private func parseClauses(until symbol: Token.Symbol) -> [ClauseLiteral]? {
        var clauses: [ClauseLiteral] = []
        _ = getNext(whenNextIs: ClauseLiteral.joukouga + ClauseLiteral.joukouwa, matchAll: false)   // 条項が、(条項は、)
        while nextToken != .symbol(symbol) && !nextToken.isEof {
            var functionParams: ParameterClauseLiteral?
            var getterParams: ParameterClauseLiteral?
            var setterParams: ParameterClauseLiteral?
            //
            skipNextEols(suppress: symbol == .EOL)
            let isTyped = getNext(whenNextIs: TypeLiteral.katano)   // 型の要素(メンバー)か
            getNext()
            let ident = Identifier(from: currentToken)
            guard getNext(whenNextIs: DefineStatement.wa) else {
                error(message: "規約で、条項の解析に失敗した。")
                return nil
            }
            _ = getNext(whenNextIs: .COMMA)
            getNext()
            let token = currentToken
            switch token {
            case .STRING(_):                // 型(文字列)のチェック
                break
            case .keyword(.FUNCTION):       // 関数(引数チェック)
                guard let parsed = FunctionLiteralParser(parser).parse() as? FunctionLiteral else {
                    error(message: "規約で、関数の「入力が〜」の解析に失敗した。")
                    return nil
                }
                if !parsed.functions.isEmpty {
                    functionParams = ParameterClauseLiteral(
                        parameters: parsed.functions.array.first?.parameters ?? [],
                        signature:  parsed.functions.array.first?.signature)
                }
            case .keyword(.COMPUTATION):    // 算出(引数チェック)
                guard let parsed = ComputationLiteralParser(parser).parse() as? ComputationLiteral else {return nil}
                if !parsed.getters.isEmpty {
                    getterParams = ParameterClauseLiteral(
                        parameters: parsed.getters.array.first?.parameters ?? [],
                        signature:  parsed.getters.array.first?.signature)
                }
                if !parsed.setters.isEmpty {
                    setterParams = ParameterClauseLiteral(
                        parameters: parsed.setters.array.first?.parameters ?? [],
                        signature:  parsed.setters.array.first?.signature)
                }
            default:
                error(message: "規約で、条項の解析に失敗した。")
                return nil
            }
            clauses.append(ClauseLiteral(
                identifier: ident,
                type: token.literal,
                functionParams: functionParams,
                getterParams: getterParams,
                setterParams: setterParams,
                isTypeMember: isTyped))
            _ = getNext(whenNextIs: .PERIOD)
            skipNextEols(suppress: symbol == .EOL)
        }
        return clauses
    }
}
struct TypeLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        if previousToken == .particle(.NO) {return Identifier(from: currentToken)}  // 〜の型：識別子として振る舞う
        let token = parseHeader()
        let endOfType: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL
        skipNextEols(suppress: endOfType == .EOL)
        // Protocols block
        guard let protocols = parseProtocols() else {return nil}
        skipNextEols(suppress: endOfType == .EOL)
        // Type member block
        var members: BlockStatement?
        switch parseOptionalBlock(of: TypeLiteral.typemembers, in: token.literal) {
        case .success(let block):
            members = block
            skipNextEols(suppress: endOfType == .EOL)
        case .failure(_):
            return nil
        }
        // Initializers block
        var initializers: FunctionBlocks
        switch parseFunctionBlocks(of: TypeLiteral.syokika, in: token.literal) {
        case .success(let inits):
            initializers = inits
            skipNextEols(suppress: endOfType == .EOL)
        case .failure(_):
            return nil
        }
        // Body block
        var body: BlockStatement?
        if !isEndOfBlock(of: endOfType) {
            _ = getNext(whenNextIs: ExpressionStatement.hontaiga + ExpressionStatement.hontaiwa, matchAll: false)   // 本体が、(本体は、)
            let endSymbol: Token.Symbol = 
                getNext(whenNextIs: .LBBRACKET) || (endOfType == .RBBRACKET) ?
                .RBBRACKET : .EOL
            body = BlockStatementParser(parser, symbol: endSymbol).blockStatement
            if body == nil {
                error(message: "型で、「本体が、〜」の解析に失敗した。")
                return nil
            }
        }
        return TypeLiteral(token: token, protocols: protocols, typeMembers: members, initializers: initializers, body: body)
    }
}
struct EnumLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = parseHeader()
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .PERIOD
        elementsCount.up(to: endSymbol)
        // 要素の解析
        guard let elements: [Statement] = parseElements(of: token, with: endSymbol) else {
            error(message: "列挙で、「要素が、〜」の解析に失敗した。")
            return nil
        }
        elementsCount.down(to: endSymbol)
        guard parseEndOfElementsLiteral(with: endSymbol) else {return nil}
        return EnumLiteral(token: token, elements: elements)
    }
}
struct ArrayLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = parseHeader()
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .PERIOD
        elementsCount.up(to: endSymbol)
        // 要素の解析
        guard let elements: [ExpressionStatement] = parseElements(of: token, with: endSymbol) else {
            error(message: "配列で、「要素が、〜」の解析に失敗した。")
            return nil
        }
        elementsCount.down(to: endSymbol)
        guard parseEndOfElementsLiteral(with: endSymbol) else {return nil}
        return ArrayLiteral(token: token, elements: elements)
    }
}
struct DictionaryLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = parseHeader()
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .PERIOD
        elementsCount.up(to: endSymbol)
        // 要素の解析
        guard let pairs: [PairExpression] = parseElements(of: token, with: endSymbol) else {
            error(message: "辞書で、「要素が、〜」の解析に失敗した。")
            return nil
        }
        elementsCount.down(to: endSymbol)
        guard parseEndOfElementsLiteral(with: endSymbol) else {return nil}
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
        if getNext(whenNextIs: CaseExpression.soreigai) {   // それ以外
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
            guard let expression = ExpressionParser(parser).parse() else {
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
/// 条件演算「よって」
/// <条件>(か)によって、<式１>か<式２>
struct ConditionalOperationParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> (any Expression)? {
        let token = currentToken                            // よって
        _ = getNext(whenNextIs: .COMMA)                     // (、)
        getNext()
        guard let consequence = ExpressionParser(parser).parse() else {
            error(message: "「(か)によって」のに続く式の解析に失敗した。")
            return nil
        }
        guard getNext(whenNextIs: ConditionalOperation.ka) else {   // か
            error(message: "「(か)によって」の後続に「か」が見つからない。")
            return nil
        }
        _ = getNext(whenNextIs: .COMMA)                     // (、)
        getNext()
        guard let alternative = ExpressionParser(parser).parse() else {
            error(message: "「(か)によって」の「か」に続く式の解析に失敗した。")
            return nil
        }
        return ConditionalOperation(token: token, consequence: consequence, alternative: alternative)
    }
}
///「反復」で始まる式を解析する。
/// 1. <数値>から<数値>まで(<数値>ずつ)反復【入力が<(カウンターの)識別子>、<処理>】
/// 2. 反復【条件が<条件式>の間、<処理>】
/// 3. 反復【<処理>】（処理を中止するには、「中止(する)」を使用する)
/// 4. <配列、辞書>を反復【入力が<(要素の）識別子>、<処理>】
struct LoopExpressionParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = parseHeader()
        let endSymbol: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL
        // Prameter block
        guard let (identifiers, _) = parseParameterBlock(in: token.literal) else {return nil}
        // Condition block
        guard let condition = parseCondition(endSymbol: endSymbol) else {
            error(message: "反復で、「条件が〜の間、」の解析に失敗した。")
            return nil
        }
        // Body block
        _ = getNext(whenNextIs: LoopExpression.syoriga + LoopExpression.syoriwa, matchAll: false)   // 処理が、(処理は、)
        guard let body = BlockStatementParser(parser, symbol: endSymbol).blockStatement else {
            error(message: "反復で、処理の解析に失敗した。")
            return nil
        }
        return LoopExpression(token: token, parameters: identifiers, condition: condition, body: body)
    }
    private func parseCondition(endSymbol: Token.Symbol) -> [Expression]? {
        guard getNext(whenNextIs: LoopExpression.condition) else {return []}    // 空のパラメータ
        _ = getNext(whenNextIs: ExpressionStatement.ga + ExpressionStatement.wa, matchAll: false)   // 条件が、(条件は、)
        _ = getNext(whenNextIs: .COMMA)
        getNext()
        var expressions: [Expression] = []
        while true {
            skipEols()
            guard let expression = ExpressionParser(parser).parse() else {
                error(message: "反復で、条件式の解析に失敗した。")
                return nil
            }
            expressions.append(expression)
            _ = getNext(whenNextIs: LoopExpression.aida)
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
        guard let right = ExpressionParser(parser, precedence: precedence).parse() else {
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
