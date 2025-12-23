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
        var tokens: [Token] = []
        var token = lexer.getNext()
        while token != Lexer.EoT {  // lexerからtokensを取り出す
            tokens.append(token)
            token = lexer.getNext()
        }
        // コピーしたParserで、仮の解析をする。(途中で失敗したらfalse)
        let tempParser = Parser(from: self.parser)
        for token in tokens {
            guard tempParser.getNext(whenNextIs: token) || !matchAll else {return false}
        }
        // 一致した場合、実際の解析を進める。
        tokens.forEach {_ = getNext(whenNextIs: $0)}
        return true
    }
    /// 入力が「<keyword>が、(は、)」に該当すれば解析位置を進め、trueを返す。
    /// - Parameters:
    ///   - keyword: チェックする文字列
    /// - Returns: 該当しなければfalseを返す。
    func getNext(whenNextKeywordIs keyword: String) -> Bool {
        guard getNext(whenNextIs: keyword) else {return false}
        _ = getNext(whenNextIs: ExpressionStatement.ga + ExpressionStatement.wa, matchAll: false)   // keywordが、(keywordは、)
        _ = getNext(whenNextIs: .COMMA)
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
        guard getNext(whenNextKeywordIs: FunctionBlock.input) else {return []}  // 入力部無し
        var parameters: [(Identifier, String, ExpressionStatement?)] = []
        var foundTerminator = false                             // 既定値式中に終端文字があったか
        repeat {
            getNext()
            let identifier = Identifier(from: currentToken)     // 識別子
            let format = getNext(whenNextIs: .string) ?
            currentToken.literal : ""           // 入力形式
            var value: ExpressionStatement? = nil
            if getNext(whenNextIs: .WA) {                       // 既定値あり
                if format.hasThreeDots {
                    error(message: "可変長識別子(\(identifier.value))に既定値を指定することはできません。")
                    return nil
                }
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
    private func removeTerminators(from expressions: inout [Expression]) -> Bool {
        _ = remove(lastParticle: .TO, from: &expressions)       //「と」を除外
        if expressions.last?.tokenLiteral == FunctionBlock.ari {
            expressions.removeLast()                            //「あり」を除外
        }
        return remove(lastParticle: .DE, from: &expressions)    //「で」を検出、除外
    }
    func parseSignature(of strings: [String], _ values: [ExpressionStatement?]) -> InputFormat {
        let formats = strings.map { string in
            var type = "", particle = ""
            let threeDots = string.getThreeDots()
            let lexer = Lexer(string.removedThreeDots)
            var token = lexer.getNext()
            if token.isIdent || token.isKeyword {type = token.literal;token = lexer.getNext()}
            if token.isParticle                 {particle = token.literal;token = lexer.getNext()}
            return InputFormat.Format(type: type, particle: particle, threeDots: threeDots)
        }
        let number = formats.contains {$0.hasThreeDots} ? nil : strings.count
        return InputFormat(numberOfInputs: number, formats: formats, values: values)
    }
    /// 返り値の型の解析
    /// - 形式: 出力が、「型名」と...、であり、
    func parseReturnTypes() -> [String]? {
        guard getNext(whenNextKeywordIs: FunctionBlock.output) else {return []} // 出力部なし
        var types: [String] = []
        repeat {
            getNext()
            guard currentToken.isString else {
                error(message: "出力の型が間違っている。(\(nextToken))")
                return nil
            }
            types.append(currentToken.literal)
            _ = getNext(whenNextIs: .TO)                        // (と)
            _ = getNext(whenNextIs: .COMMA)                     // (、)
            guard !nextToken.isEof else {return nil}
        } while !isEndOfParameter
        _ = getNext(whenNextIs: FunctionBlock.ari)              // (あり)
        _ = getNext(whenNextIs: .COMMA)                         // (、)
        return types
    }
    func parseProtocols() -> [String]? {
        var protocols: [String] = []
        skipNextEols()
        _ = getNext(whenNextIs: ProtocolLiteral.junkyosuru)     // 準拠する
        if getNext(whenNextKeywordIs: ProtocolLiteral.kiyaku) { // 規約は、(規約が、)
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
    /// - Parameters:
    ///     - token:  要素群の種別を表すトークン
    ///     - endSymbol: 期待する終端
    /// - Returns: 解析した要素の配列(もしくは、nil : エラー)
    func parseElements<T>(in token: Token, with endSymbol: Token.Symbol) -> [T]? {
        if getNext(whenNextIs: endSymbol) {
            return []                                           // 空の要素
        }
        if endSymbol == .PERIOD && nextToken.isEol {            // 型直後の開業
            error(message: "\(token.literal)の要素が空である場合は、句点「。」が必要。")
            return nil
        }
        skipNextEols()
        _ = getNext(whenNextIs: ExpressionStatement.yousoga + ExpressionStatement.yousowa, matchAll: false) // 要素が、(要素は、)
        skipNextEols()
        var commaNotFound: Bool = false                         // ブロック内で、要素の後に「、」が見つからない
        var elements: [T] = []
        while true {
            if isTerminator(nextToken, expected: endSymbol) {   // 次が終端ならば解析終了
                break
            }
            getNext()
            skipEols()
            if currentToken.isSymbol(.RBBRACKET) {              // ブロック終了なら即時リターン
                return elements
            }
            guard let parsed: T = parseElement(of: token) else {
                error(message: "\(token.literal)で、要素の解釈に失敗した。")
                return nil
            }
            elements.append(parsed)
            //
            if !getNext(whenNextIs: .COMMA) {                   // 次が「、」以外ならば解析終了
                if (nextToken.isEol || nextToken.isPeriod) && endSymbol == .RBBRACKET  {
                    commaNotFound = true
                }
                break
            }
        }
        // 後処理(endSymbolをcurrentにする。)
        while nextToken.isEol ||
              (nextToken.isPeriod && endSymbol != .PERIOD) {
            getNext()
        }
        let endSymbolFound = getNext(whenNextIs: endSymbol)
        
        if commaNotFound && !endSymbolFound {   // ブロック内で、要素の後に区切り「、」が見つからず、終端でない
            error(message: "要素の区切りには、「、」が必要。")
            return nil
        }
        
        return elements
    }
    private func isTerminator(_ token: Token, expected: Token.Symbol) -> Bool {
        token.isSymbol(expected) ||             // 期待する終端
        token.isSymbol(.RBBRACKET) ||           // 「】」は終端
        (expected == .PERIOD && token.isEol) || // 終端「。」待ちならば、EOLは終端
        token.isEof
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
    func parseInputBlock(in type: String) -> ([Identifier], InputFormat)? {
        guard let paramenters = parseParameters() else {
            error(message: "\(type)で、「入力が〜」の解析に失敗した。")
            return nil
        }
        return (paramenters.map {$0.0}, 
                parseSignature(of: paramenters.map {$0.1}, paramenters.map {$0.2})
        )
    }
    /// 出力部： 出力が(は)「(<型> )」…であり、
    /// - Returns: 型名の配列もしくはエラー(nil)
    func parseOutputBlock(in type: String) -> [String]? {
        guard let types = parseReturnTypes() else {
            error(message: "\(type)で、「出力が〜」の解析に失敗した。")
            return nil
        }
        if types.contains(where: \.isEmpty), types.count > 1 {
            error(message: "出力が「」で、複数の型を指定することはできない。")
            return nil
        }
        return types
    }
    /// 定義部： <ブロック名>は(が)、【<定義>】
    /// 定義部から<定義>を取り出し、ブロック文として返す。
    /// - Returns: ブロック文、定義無し(nil)、もしくはエラー
    func parseOptionalBlock(of blockname: String, in typename: String) -> Result<BlockStatement?, FunctionBlockError> {
        guard getNext(whenNextKeywordIs: blockname) else {return .success(nil)}
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
    /// 関数部：【入力が(は)、〜。出力が(は)、〜。本体が、〜。】
    /// - Returns: 関数ブロック、もしくはエラー
    func parseFunctionBlock(in typename: String, endOfBlock: Token.Symbol? = nil, isOverloaded: Bool = false) -> Result<FunctionBlock, FunctionBlockError> {
        let endSymbol: Token.Symbol = (endOfBlock == nil) ?
        getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL :
        endOfBlock!
        // Prameter block
        guard let (identifiers, signature) = parseInputBlock(in: typename) else {return .failure(FunctionBlockError.input)}
        guard let returnTypes = parseOutputBlock(in: typename) else {return .failure(FunctionBlockError.output)}
        // Body block
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
            paramForm: signature,
            returnTypes: returnTypes,
            body: body,
            isOverloaded: isOverloaded
        ))
    }
    func parseFunctionBlocks(of name: String, in typename: String) -> Result<FunctionBlocks, FunctionBlockError> {
        var functionBlocks = FunctionBlocks()
        while getNext(whenNextKeywordIs: name) {
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
    /// 要素群の終端解析
    /// ＊：呼び元の要素解析で、終端まで解析を進めること！
    /// - Parameter endSymbol: 期待する終端
    /// - Returns: 解析結果
    func parseEndOfElements(with endSymbol: Token.Symbol) -> Bool {
        switch endSymbol {
        case .PERIOD:
            if currentToken.isPeriod || currentToken.isEof {return true}
            if currentToken.isEol {
                if !nextToken.isEof {
                    print("警告：要素の終りの句点「。」が見つかりません。要素が複数行にまたがる場合は、ブロック「【】」で囲ってください。")
                }
                return true
            }
            error(message: "要素の解析で、終端「。」が見つからない。")
        case .RBBRACKET:
            if currentToken.isSymbol(.RBBRACKET) {
                // 余分な「】」が続く場合は読み進める
                skipNextRBBrackets()
                return true
            }
            error(message: "要素の解析で、終端「】」が見つからない。")
        default:
            error(message: "要素の解析で、期待しない終端「\(endSymbol.rawValue)」が見つかった。")
        }
        return false
    }
    private func skipNextRBBrackets() {
        while elementsCount.value(of: .RBBRACKET) < 1 &&    // 要素の解析が終了している
                blockCount.value(of: .RBBRACKET) < 1 &&     // ブロックの終端ではない
                nextToken.isSymbol(.RBBRACKET) {
            getNext()
        }
    }
}
enum FunctionBlockError : Error {
    case input, output, body
    var message: String {
        switch self {
        case .input:        return "(入力の解析エラー)"
        case .output:       return "(出力の解析エラー)"
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
        let identifier = Identifier(from: currentToken.literal)
        parser.insert(identifier.value)     // 識別子をLexerに登録
        getNext()
        let token = currentToken            // 「は」「とは」
        let syntax = (token.literal == DefineStatement.towa) ? syntax1 : syntax2
        _ = getNext(whenNextIs: .COMMA)     // 読点(、)を読み飛ばす
        let isExtended = getNext(whenNextIs: DefineStatement.further)   // 「さらに、」
        _ = getNext(whenNextIs: .COMMA)
        getNext()
        guard var parsed = ExpressionStatementParser(parser).parse() as? ExpressionStatement else {
            error(message: "\(syntax)で、式の解釈に失敗した。")
            return nil
        }
        _ = getNext(whenNextIs: DefineStatement.koto)
        if !isEndOfBlock {
            _ = getNext(whenNextIs: DefineStatement.dearu + DefineStatement.desu, matchAll: false)
            _ = getNext(whenNextIs: .PERIOD)
            skipNextEols()                  // EOLの前で解析を停止する。
        }
        if var function = parsed.expressions.first as? FunctionLiteral {
            function.name = identifier.value// 関数の名前を記録
            function.function.isOverloaded = isExtended
            parsed.expressions[0] = function// DefineStatement.valueに反映する。
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
            // 要素代入する対象の属格を２つの句に分割する
            if isElementAssignPredicate(expression) {
                splitElementAssignTarget(in: &expressions)
            }
            // LHSに代入する述語の場合、LHS対象の識別子に印をつける
            if isLhsAssignPredicate(expression) {
                setLhsFlag(into: &expressions)
            }
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
    /// 式が左辺(代入される対象の識別子)を持つ述語かどうか
    private func isLhsAssignPredicate(_ exp: Expression) -> Bool {
        guard let predicate = exp as? PredicateExpression else {
            return false
        }
        return predicate.token.hasLhsIdentifier
    }
    /// 式が要素代入をする述語かどうか
    private func isElementAssignPredicate(_ exp: Expression) -> Bool {
        guard let predicate = exp as? PredicateExpression else {
            return false
        }
        return predicate.token.isKeyword(.ASSIGN)
    }
    /// 左辺を持つ式配列の対象識別子に、左辺フラグを立てる
    private func setLhsFlag(into exps: inout [Expression]) {
        var candidates: [Int] = []  // 左辺識別子の候補
        for i in 0..<exps.count {
            // 関係の無い述語であれば、候補をキャンセル
            if let predicate = exps[i] as? PredicateExpression,
               !predicate.token.hasLhsIdentifier {
                candidates = []
                continue
            }
            guard let phrase = exps[i] as? PhraseExpression,
                  phrase.left is Identifier else {
                continue
            }
            switch phrase.token {
            case .particle(.TO):
                candidates.append(i)
                continue
            case .particle(.NO):
                candidates = [i]
                continue
            case .particle(.NI):
                if let idx = candidates.first,
                   let phrase = exps[idx] as? PhraseExpression,
                       phrase.token == .particle(.NO) { // aのbに → a/
                        break
                }
                candidates.append(i)
            default:
                continue
            }
            // 確定した候補の識別子を、[Expression]に代入する。
            for idx in candidates {
                guard let targetPhrase = exps[idx] as? PhraseExpression,
                      let targetIdent = targetPhrase.left as? Identifier else {
                    continue
                }
                let newIdent = Identifier(token: targetIdent.token, value: targetIdent.value, isLhs: true)
                exps[idx] = PhraseExpression(token: targetPhrase.token, left: newIdent)
            }
            candidates = []
        }
    }
    /// 要素代入する対象の属格を２つの句に分割する
    private func splitElementAssignTarget(in exps: inout [Expression]) {
        for i in 0..<exps.count {
            // 関係ない述語であれば、候補をキャンセル
            if let predicate = exps[i] as? PredicateExpression,
               !predicate.token.isKeyword(.ASSIGN) {
                continue
            }
            guard let genitive = exps[i] as? GenitiveExpression,
                  let rightPhrase = genitive.right as? PhraseExpression,
                  rightPhrase.token.isParticle(.NI)  else {
                continue
            }
            // 属格を句に分割
            exps[i] = PhraseExpression(token: Token(.NO), left: genitive.left)
            exps.insert(rightPhrase, at: i + 1)
            break
        }
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
        parser.switchCase.enter()
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
        if parser.switchCase.isActive {
            parser.errors.append(parser.switchCase.defaultError)
            return nil
        }
        parser.switchCase.leave()
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
    case lowest = 1, block, genitive, or, and
    static func < (lhs: Self, rhs: Self) -> Bool {lhs.rawValue < rhs.rawValue}
    static let precedences: [Token.TokenType: Self] = [
        .keyword(.OR):          .or,
        .particle(.NO):         .genitive,
        .symbol(.LBBRACKET):    .block,
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
        case .keyword(.IDENTIFIER),.keyword(.FILE),.keyword(.MEMBER),.keyword(.OUTER),.keyword(.RESERVEDWORD):
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
    func parse() -> Expression? {
        let token = currentToken
        let names = token.literal.components(separatedBy: EnumeratorLiteral.dot)
        if names.count == 2 {
            let enumerator = EnumeratorLiteral(token: token, type: names[0], name: names[1])
            return parseRangeExpression(with: enumerator)
        }
        return parseRangeExpression(with: Identifier(from: token))
    }
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
/// ラベル(キーワード)と、後続の値(または識別子)を記憶する。
/// ※：例：ファイル「text.txt」、識別子『割った余り』、外部「甲」
/// ※：キーワードに識別子を続けると、合成された識別子となるので、『』で明示的に表記する必要がある。
struct LabelExpressionParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = currentToken                // ラベル
        if token.isKeyword(.OUTER) {            // 外部(outer)識別子
            getNext()
            return Label(token: token, value: Token(ident: currentToken.literal))
        } else
        if token.isKeyword(.RESERVEDWORD) {     // 予約語
            getNext()
            guard let keyword = Token.Keyword(rawValue: currentToken.literal), 
                    Token.redefinables.contains(keyword) else {
                error(message: "「\(currentToken.literal)」は、再定義可能な述語ではない。")
                return nil
            }
            return Label(token: token, value: Token(keyword: keyword))
        }
        switch nextToken.type {
        case .string,.ident,.int,.keyword(.TRUE),.keyword(.FALSE):
            break
        default:
            error(message: "「\(token.literal)」の後続が<文字列>、<識別子>、<真>、<偽>または<数値>ではなかった。)")
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
            return FunctionLiteral(token: token, function: function)
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
    /// 条項群の解析
    private func parseClauses(until symbol: Token.Symbol) -> [ClauseLiteral]? {
        var clauses: [ClauseLiteral] = []
        _ = getNext(whenNextIs: ClauseLiteral.joukouga + ClauseLiteral.joukouwa, matchAll: false)   // 条項が、(条項は、)
        while nextToken != .symbol(symbol) && !nextToken.isEof {
            guard let clauseGroup = parseClauseGroup(until: symbol) else {return nil}
            clauses.append(contentsOf: clauseGroup)
        }
        return clauses
    }
    /// 各条項の解析
    /// 1. (型の)<識別子>は、<型>
    /// 2. (型の)<識別子>は、関数【<入力定義>。<出力定義>】
    /// 3. (型の)<識別子>は、算出【<入力定義>。<出力定義>】
    /// 4. 初期化は(が)、【<入力定義>。<出力定義>】
    private func parseClauseGroup(until symbol: Token.Symbol) -> [ClauseLiteral]? {
        skipNextEols(suppress: symbol == .EOL)
        //
        let isStatic = getNext(whenNextIs: TypeLiteral.katano)  // 型の<識別子>
        var token = nextToken
        var ident = Identifier(from: token)
        switch token {
        case .IDENT:
            getNext()
            ident = Identifier(from: currentToken)              // <識別子>は、
            if isStatic && ident.value == "要素" {
                error(message: "規約で、「型の要素は、【〜】」の定義はできない。個別に「型の<識別子>は、〜」の定義が必要。")
                return nil
            }
            guard getNext(whenNextIs: .WA) else {
                error(message: "規約で、条項「<識別子>は」の解析に失敗した。")
                return nil
            }
            _ = getNext(whenNextIs: .COMMA)
            _ = getNext(whenNextIs: DefineStatement.further)    // 「さらに」は読み飛ばす
            _ = getNext(whenNextIs: .COMMA)
            getNext()
            token = currentToken
        case .keyword(.INITIALIZATION):
            if isStatic {
                error(message: "型の初期化はできない(「型の」は不要)。")
                return nil
            }
        default:
            error(message: "規約で、条項(トークン：\(token.literal))の解析に失敗した。")
            return nil
        }
        guard let signatureKinds = parseClauseContents(from: token) else {return nil}
        _ = getNext(whenNextIs: .PERIOD)
        skipNextEols(suppress: symbol == .EOL)
        
        return signatureKinds.map { kind in
            ClauseLiteral(
                isTypeMember: isStatic,
                identifier: ident,
                type: token.literal,
                kind: kind,
            )
        }
    }
    /// 条項の型毎の入出力の解析
    private func parseClauseContents(from token: Token) -> [SignatureKind]? {
        switch token {
        case .STRING(_):                // 型(文字列)
            return [.none]
        case .keyword(.FUNCTION):       // 関数
            guard let parsed = FunctionLiteralParser(parser).parse() as? FunctionLiteral else {
                error(message: "規約で、関数定義の解析に失敗した。")
                return nil
            }
            let sig = FunctionSignature(
                parameters:  parsed.function.parameters,
                paramForm:   parsed.function.paramForm,
                returnTypes: parsed.function.returnTypes
            )
            return [.function(sig)]
        case .keyword(.COMPUTATION):    // 算出
            guard let parsed = ComputationLiteralParser(parser).parse() as? ComputationLiteral else {
                error(message: "規約で、算出定義の解析に失敗した。")
                return nil
            }
            return makeComputationSignatures(from: parsed)
        case .keyword(.INITIALIZATION): // 初期化
            switch parseFunctionBlocks(of: token.literal, in: token.literal) {
            case .success(let blocks):
                return blocks.array.map {
                    .initializer(FunctionSignature(
                        parameters: $0.parameters,
                        paramForm:  $0.paramForm,
                        returnTypes:$0.returnTypes
                    ))
                }
            case .failure(_):
                return nil
            }
        default:
            error(message: "規約で、条項の解析に失敗した。(未対応の型: \(token.literal))")
            return nil
        }
    }
    /// 算出内の多重定義(FunctionBlocks) → [SignatureKind]変換
    private func makeComputationSignatures(from literal: ComputationLiteral) -> [SignatureKind]? {
        var getters = literal.getters.array.map {Optional($0)}
        var setters = literal.setters.array.map {Optional($0)}
        let n = max(getters.count, setters.count)
        guard n > 0 else {
            error(message: "規約の算出には、取得か設定の少なくともいずれかの定義が必要。")
            return nil
        }
        // 足りない分をnilで埋める
        getters += Array(repeating: nil, count: n - getters.count)
        setters += Array(repeating: nil, count: n - setters.count)
        // [FunctionBlock?,FunctionBlock?] → [.computation(FunctionSignature?,FunctionSignature?)]
        return zip(getters, setters).map { (getter, setter) in
                .computation(
                    getter: getter.map {
                        FunctionSignature(
                            parameters: $0.parameters,
                            paramForm: $0.paramForm,
                            returnTypes: $0.returnTypes)
                    },
                    setter: setter.map {
                        FunctionSignature(
                            parameters: $0.parameters,
                            paramForm: $0.paramForm,
                            returnTypes: $0.returnTypes)
                    })
        }
    }
}
struct TypeLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        // exception proc
        if previousToken.isParticle(.NO) || previousToken.isKeyword(.ITS) {
            return Identifier(from: currentToken)                       // 〜の型：識別子として振る舞う
        }
        if nextToken.isParticle(.NO) {
            getNext(); return StringLiteral(from: TypeLiteral.katano)   // 型の：文字列として振る舞う
        }
        // Header proc
        let token = parseHeader()
        let endOfType: Token.Symbol = getNext(whenNextIs: .LBBRACKET) ? .RBBRACKET : .EOL
        skipNextEols(suppress: endOfType == .EOL)
        // Protocols block
        guard let protocols = parseProtocols() else {return nil}
        skipNextEols(suppress: endOfType == .EOL)
        // Type member block
        var members: BlockStatement?
        switch parseOptionalBlock(of: TypeLiteral.typemembers, in: token.literal) { // 「型の要素は、【<要素定義>】
        case .success(let block):
            members = block
            skipNextEols(suppress: endOfType == .EOL)
        case .failure(_):
            return nil
        }
        guard parseTypeMembers(with: &members) else {                               // 型の<識別子>を取り込む
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
    /// 型の<識別子>は、<定義>を型の要素に取り込む
    private func parseTypeMembers(with typeMembers: inout BlockStatement?) -> Bool {
        var definitions = [DefineStatement]()
        while getNext(whenNextIs: TypeLiteral.katano) {
            getNext()   // DefStatemntParserは、currentTokenで処理をするため、1つ進める
            guard let definition = DefStatementParser(parser).parse() as? DefineStatement else {
                error(message: "型で、「型の<識別子>は、〜」の解析に失敗した。")
                return false
            }
            definitions.append(definition)
        }
        if !definitions.isEmpty {
            typeMembers = BlockStatement(
                token: typeMembers?.token ?? definitions.first!.token,
                statements: (typeMembers?.statements ?? []) + definitions)
        }
        return true
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
        guard let elements: [Statement] = parseElements(in: token, with: endSymbol) else {
            error(message: "列挙で、「要素が、〜」の解析に失敗した。")
            return nil
        }
        elementsCount.down(to: endSymbol)
        guard parseEndOfElements(with: endSymbol) else {return nil}
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
        guard let elements: [ExpressionStatement] = parseElements(in: token, with: endSymbol) else {
            error(message: "配列で、「要素が、〜」の解析に失敗した。")
            return nil
        }
        elementsCount.down(to: endSymbol)
        guard parseEndOfElements(with: endSymbol) else {return nil}
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
        guard let pairs: [PairExpression] = parseElements(in: token, with: endSymbol) else {
            error(message: "辞書で、「要素が、〜」の解析に失敗した。")
            return nil
        }
        elementsCount.down(to: endSymbol)
        guard parseEndOfElements(with: endSymbol) else {return nil}
        return DictionaryLiteral(token: token, pairs: pairs)
    }
}
struct PredicateExpressionParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        if currentToken.isKeyword(.ITS) && !(nextToken.isIdent || nextToken.isKeyword(.TYPE)) {
            error(message: "「\(nextToken.literal)」は属性名ではない。")
            return nil
        }
        return PredicateExpression(token: currentToken)
    }
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
        _ = getNext(whenNextIs: .COMMA)                     // 読点(、)を読み飛ばす
        _ = getNext(whenNextIs: .EOL)                       // EOLを読み飛ばす
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
        // Input block
        guard let (identifiers, _) = parseInputBlock(in: token.literal) else {return nil}
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
        guard getNext(whenNextKeywordIs: LoopExpression.condition) else {return []}    // 空のパラメータ
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
        case .particle(.NO):        return GenitiveExpressionParser(parser, with: left)
        case .symbol(.LBBRACKET):   return CallExpressionParser(parser, with: left)
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
        let precedence = Precedence[currentToken.type]
        getNext()
        guard let right = ExpressionParser(parser, precedence: precedence).parse() else {
            error(message: "中間置式(\(op)で、右辺の解析に失敗した。")
            return nil
        }
        return OrExpression(token: token, left: left, right: right)
    }
}
struct CallExpressionParser : ExpressionParsable {
    init(_ parser: Parser, with left: Expression?) {self.parser = parser; self.left = left}
    let parser: Parser
    let left: Expression?
    func parse() -> (any Expression)? {
        let token = currentToken
        _ = getNext(whenNextIs: CallExpression.arguments)   // 引数が、
        guard let caller = left else {
            error(message: "呼び出し式で、左辺の解析に失敗した。")
            return nil
        }
        guard let block = BlockStatementParser(parser, symbol: .RBBRACKET).blockStatement else {
            error(message: "呼び出し式で、引数の解析に失敗した。")
            return nil
        }
        var arguments: [DefineStatement] = []
        for statement in block.statements {
            guard let define = statement as? DefineStatement else {
                error(message: "呼び出し式の引数が定義文で定義されていない。")
                return nil
            }
            arguments.append(define)
        }
        return CallExpression(token: token, target: caller, arguments: arguments)
    }
}
struct GenitiveExpressionParser : ExpressionParsable {
    init(_ parser: Parser, with left: Expression?) {self.parser = parser; self.left = left}
    let parser: Parser
    let left: Expression?
    func parse() -> (any Expression)? {
        let token = currentToken
        guard let left = left else {
            error(message: "属格で、左式の解析に失敗した。")
            return nil
        }
        if nextToken.isPeriod || nextToken.isEol || nextToken.isEof {
            return PhraseExpression(token: currentToken, left: left)
        }
        let precedence = Precedence[currentToken.type]
        getNext()
        guard let right = ExpressionParser(parser, precedence: precedence).parse() else {
            error(message: "属格で、右式の解析に失敗した。")
            return nil
        }
        if let exp = right as? CaseExpression {         // 場合文の「それ以外は」のチェック
            parser.switchCase.isActive = (exp.alternative == nil)
        }
        if let phrase = right as? PhraseExpression {    // 属格の右辺格処理
            switch phrase.token {
            case .particle(.WA):
                _ = getNext(whenNextIs: .COMMA)
                getNext()
                guard let parsed = ExpressionStatementParser(parser).parse() as? ExpressionStatement else {
                    error(message: "属格で、値式の解釈に失敗した。")
                    return nil
                }
                return GenitiveExpression(token: token, left: left, right: right, value: parsed)
            default:
                break
            }
        }
        return GenitiveExpression(token: token, left: left, right: right)
    }
}
// MARK: - postfix expression parsers and those instance factory
struct PostfixExpressionParserFactory {
    static func create(from parser: Parser, with left: Expression?) -> ExpressionParsable? {
        switch parser.nextToken.type {   // nextTokenに続くトークンを解析する解析器
        case .particle(.NO):    return nil
        case .particle(_):      return PhraseExpressionParser(parser, with: left)
        default:                return nil
        }
    }
}
struct PhraseExpressionParser : ExpressionParsable {
    init(_ parser: Parser, with left: Expression?) {self.parser = parser; self.left = left}
    let parser: Parser
    let left: Expression?
    func parse() -> Expression? {
        let token = currentToken
        if token.isParticle(.KO) && nextToken.isParticle {getNext()}
        guard let left = left else {
            error(message: "「\(token.literal)」格の左辺(式)の解析に失敗した。")
            return nil
        }
        return PhraseExpression(token: token, left: left)
    }
}
