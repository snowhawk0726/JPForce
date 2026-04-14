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
    func getNext(whenNextIs expected: String, matchAll: Bool = true) -> Bool {
        parser.getNext(whenNextIs: expected, matchAll: matchAll)
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
    func getNext(kind: BlockKind) -> Bool {
        switch kind {
        case .explicit:
            return getNext(whenNextIs: .RBBRACKET)
        case .implicit:
            return getNext(whenNextIs: .EOL) || getNext(whenNextIs: .PERIOD)
        }
    }
    func skipEols() {parser.skipEols()}
    func skipNextEols(blockKind: BlockKind = .explicit) {
        while blockKind == .explicit && nextToken.isEol {getNext()}
    }
    /// エラー出力
    func error(message: String) {
        error(message: message, at: currentToken)
    }
    func error(message: String, at token: Token) {
        parser.errors.append(message + (currentToken.isIllegal ? "" : "(解析位置: \(token.literal))"))
    }
    /// ブロック(入れ子)制御
    func openBlock(kind: BlockKind, with token: Token? = nil) {
        parser.blockStack.append(BlockFrame(
            kind: kind,
            token: token
        ))
    }
    func openBlock(kind: BlockKind, with string: String) {
        parser.blockStack.append(BlockFrame(
            kind: kind,
            target: string
        ))
    }
    func closeBlock(isExplicit: Bool) -> Bool {
        isExplicit ? closeExplicitBlock() : closeImplicitBlock()
    }
    func closeBlock(kind: BlockKind) -> Bool {
        switch kind {
        case .explicit:
            closeExplicitBlock()
        case .implicit:
            closeImplicitBlock()
        }
    }
    private func closeExplicitBlock() -> Bool {
        guard let top = parser.blockStack.last else {
            error(message: ParserError.unmatchedClosingBlacket.message)
            return false
        }
        guard top.kind == .explicit else {
            error(message: ParserError.mismatchedBlockClose.message)
            return false
        }
        parser.blockStack.removeLast()
        return true
    }
    private func closeImplicitBlock() -> Bool {
        if let top = parser.blockStack.last,
              top.kind == .implicit {
            parser.blockStack.removeLast()
        }
        return true
    }
    func shouldCloseExplicitBlock() -> Bool {
        if let top = parser.blockStack.last,
           top.kind == .explicit {
            return true
        }
        return false
    }
    func shouldCloseImplicitBlock() -> Bool {
        if let top = parser.blockStack.last,
           top.kind == .implicit {
            return true
        }
        return false
    }
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
                    error(message: "入力部で、既定値の式の解析に失敗しました。")
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
                error(message: "出力の型が間違っています。(\(nextToken))")
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
                    error(message: "型で、準拠する規約の型が間違っています。(\(nextToken))")
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
    func parseAuxiliaryToken() -> Token? {
        guard nextToken.isKeyword(.SURU) else {return nil}
        getNext()
        return currentToken
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
            error(message: "範囲で、範囲式の解析に失敗しました。(下限「\(lowerBound?.tokenLiteral ?? "?")」が重複しています。)")
            return nil
        }
        let upperBound = getBound(of: [Token(.MADE), Token(.LTEQUAL), Token(.UNDER)], from: rest)
        if upperBound != nil {
            guard getRest(of: rest, except: upperBound).isEmpty else {
                error(message: "範囲で、範囲式の解析に失敗しました。(上限「\(upperBound?.tokenLiteral ?? "?")」に後続の式があります。)")
                return nil
            }
        } else {
            guard rest.isEmpty else {
                error(message: "範囲で、範囲式の解析に失敗しました。(上限の形式が間違っています。)")
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
        if let e = expressions.first as? RangeLiteral {return tokens.first?.isLower == true ? e.lowerBound : e.upperBound}
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
    /// 配列・辞書・列挙の要素を解析する（要素の終わりまで）
    /// 形式： 要素が(は)、〜。、要素が(は)、【〜】
    /// - Parameters:
    ///     - token:  要素群の種別を表すトークン
    ///     - endSymbol: 期待する終端
    /// - Returns: 解析した要素の配列(もしくは、nil : エラー)
    func parseElements<T>(in token: Token, kind: BlockKind) -> [T]? {
        if getNext(kind: kind) {
            return []                                           // 空の要素
        }
        if kind == .implicit && nextToken.isEol {               // 型直後の改行
            error(message: "\(token.literal)の要素が空である場合は、句点「。」が必要です。")
            return nil
        }
        skipNextEols()
        _ = getNext(whenNextKeywordIs: ExpressionStatement.youso)   // 要素が、(要素は、)
        skipNextEols()
        var commaNotFound: Bool = false                         // ブロック内で、要素の後に「、」が見つからない
        var elements: [T] = []
        while !isTerminator(nextToken, kind: kind) {            // 次が終端ならば解析終了
            getNext()
            skipEols()
            if currentToken.isEndOfBlock {                      // ブロック終了なら即時リターン
                return elements
            }
            guard let parsed: T = parseElement(of: token) else {
                error(message: "\(token.literal)で、要素の解釈に失敗しました。")
                return nil
            }
            elements.append(parsed)
            //
            if !getNext(whenNextIs: .COMMA) {                   // 次が「、」以外ならば解析終了
                if (nextToken.isEol || nextToken.isPeriod) && kind == .explicit  {
                    commaNotFound = true
                }
                break
            }
        }
        // 後処理(endSymbolをcurrentにする。)
        while nextToken.isEol || (nextToken.isPeriod && kind != .implicit) {
            getNext()
        }
        let endSymbolFound = getNext(kind: kind)
        
        if commaNotFound && !endSymbolFound {   // ブロック内で、要素の後に区切り「、」が見つからず、終端でない
            error(message: "要素の区切りには、「、」が必要です。")
            return nil
        }
        
        return elements
    }
    private func isTerminator(_ token: Token, kind: BlockKind) -> Bool {
        switch kind {
        case .explicit:
            return token.isEndOfBlock
        case .implicit:
            return token.isEndOfStatement
        }
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
            error(message: "配列で、式の解析に失敗しました。")
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
            error(message: "列挙で、識別子の解析に失敗しました。")
            return nil
        }
        let ident = Identifier(from: token)
        if getNext(whenNextIs: .WA) {  // 値あり
            let defineToken = currentToken
            _ = getNext(whenNextIs: .COMMA)
            getNext()
            let valueToken = currentToken
            guard var expressions = parseElement() else {
                error(message: "列挙で、値の式の解析に失敗しました。")
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
            error(message: "辞書で、索引の式の解析に失敗しました。")
            return nil
        }
        _ = getNext(whenNextIs: .COMMA)
        guard remove(lastParticle: .GA, from: &expressions) else {  //「が」を取り除く
            error(message: "辞書で、索引と値の区切り「が」が見つかりません。")
            return nil
        }
        let keyToken = Token(word: expressions[0].tokenLiteral)
        let keyExpressions = ExpressionStatement(token: keyToken, expressions: expressions)
        getNext()
        // 値の解析
        guard var expressions = parseElement() else {
            error(message: "辞書で、値の式の解析に失敗しました。")
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
        if let phrase = expressions.last as? PhraseExpression, phrase.hasParticle(p) {
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
               let particle = p, phrase.hasParticle(particle) {
                return expressions      // 指定格があった
            }
            if nextToken.isEndOfElements {break}
            getNext()
        }
        return expressions
    }
    /// 要素群の終端解析
    /// ＊：呼び元の要素解析で、終端まで解析を進めること！
    /// - Parameter endSymbol: 期待する終端
    /// - Returns: 解析結果
    func parseEndOfElements(kind: BlockKind) -> Bool {
        switch kind {
        case .implicit:
            if currentToken.isPeriod || currentToken.isEof {return true}
            if currentToken.isEol {
                if !nextToken.isEof {
                    print("警告：要素の終りの句点「。」が見つかりません。要素が複数行にまたがる場合は、ブロック「【】」で囲ってください。")
                }
                return true
            }
            error(message: "要素の解析で、終端「。」が見つかりません。")
        case .explicit:
            if currentToken.isEndOfBlock {
                // 余分な「】」が続く場合は blockStack の状態を見て読み進める
                skipNextRBBracketsUsingBlockStack()
                return true
            }
            error(message: "要素の解析で、終端「】」が見つかりません。")
        }
        return false
    }
    private func skipNextRBBracketsUsingBlockStack() {
        // 余分な「】」が続く場合は、blockStack の状態を確認しながら安全に読み進める。
        // 明示ブロックをここで閉じるべきなら（shouldCloseExplicitBlock が true）、これ以上スキップしない。
        while nextToken.isSymbol(.RBBRACKET) {
            if shouldCloseExplicitBlock() {
                break
            }
            // 暗黙ブロック(implicit)がトップの場合、ここで閉じるべきではない余剰な「】」とみなし読み進める。
            getNext()
        }
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
            error(message: "\(type)で、「入力が〜」の解析に失敗しました。")
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
            error(message: "\(type)で、「出力が〜」の解析に失敗しました。")
            return nil
        }
        if types.contains(where: \.isEmpty), types.count > 1 {
            error(message: "出力が「」で、複数の型を指定することはできません。")
            return nil
        }
        return types
    }
    /// 定義部： <ブロック名>は(が)、【<定義>】
    /// 定義部から指定ブロック名を持つ<定義>を取り出し、ブロック文として返す。
    /// - Returns:
    ///     success: ブロック文、定義無し(nil)
    ///     failure: エラー
    func parseOptionalBlock(of blockname: String, in typename: String) -> Result<BlockStatement?, ParserError> {
        guard getNext(whenNextKeywordIs: blockname) else {return .success(nil)}
        if nextToken.isEndOfStatement {
            error(message: "\(typename)で、「\(blockname)」の定義が正しくありません。「\(blockname)が、」の後ろにはブロック記号`【`、もしくは文が必要です。")
            return .failure(.blockParseError)
        }
        let kind = BlockKind(isExplicit: getNext(whenNextIs: .LBBRACKET))
        guard let blockStatement = BlockStatementParser(parser, kind: kind).blockStatement else {
            error(message: "\(typename)で、「\(blockname)は、〜」の解析に失敗しました。")
            return .failure(.blockParseError)
        }
        _ = getNext(whenNextIs: .PERIOD)    // ブロックの句点を飛ばす
        return .success(blockStatement)
    }
    /// 定義ブロック内のブロックを解析
    /// - Parameters:
    ///   - name: ブロック名
    ///   - kind: ブロック種別(明示: .explicit, 暗黙: .implicit)
    ///   - typename: 定義型名
    /// - Returns: BlockStatement / nil
    func parseBlock(name: String, kind: BlockKind, in typename: String) -> Result<BlockStatement?, ParserError> {
        switch parseOptionalBlock(of: name, in: typename) {
        case .success(let block?):          // ブロック名明示
            skipNextEols(blockKind: kind)
            return .success(block)
        case .success(nil):                 // ブロック名省略
            guard !getNext(kind: kind) else {return .success(nil)}
            guard let block = BlockStatementParser(parser, kind: kind).blockStatement else {
                error(message: "\(typename)で、「\(name)は、〜」の解析に失敗しました。")
                return .failure(.blockParseError)
            }
            return .success(block)
        default:
            return .failure(.blockParseError)
        }
    }
    func isEndOfBlock(kind: BlockKind) -> Bool {
        switch kind {
        case .explicit:
            nextToken.isEndOfBlock
        case .implicit:
            nextToken.isEndOfStatement
        }
    }
    /// 関数部：【入力が(は)、〜。出力が(は)、〜。本体が、〜。】
    /// - Returns: 関数ブロック、もしくはエラー
    func parseFunctionBlock(in typename: String, kind: BlockKind, isOverloaded: Bool = false) -> Result<FunctionBlock, FunctionBlockError> {
        // Prameter block
        guard let (identifiers, signature) = parseInputBlock(in: typename) else {return .failure(.input)}
        guard let returnTypes = parseOutputBlock(in: typename) else {return .failure(.output)}
        // Body block
        switch parseBlock(name: ExpressionStatement.hontai, kind: kind, in: typename) {
        case .success(let body):
            _ = getNext(whenNextIs: .PERIOD)    // ブロックの句点を飛ばす
            return .success(FunctionBlock(
                parameters: identifiers,
                paramForm: signature,
                returnTypes: returnTypes,
                body: body,
                isOverloaded: isOverloaded
            ))
        case .failure:
            return .failure(.body)
        }
    }
    func parseFunctionBlocks(of name: String, in typename: String) -> Result<FunctionBlocks, FunctionBlockError> {
        let functionBlocks = FunctionBlocks()
        while getNext(whenNextKeywordIs: name) {
            let isOverloaded = getNext(whenNextIs: DefineStatement.further)
            _ = getNext(whenNextIs: .COMMA)
            //
            let kind = BlockKind(isExplicit: getNext(whenNextIs: .LBBRACKET))
            switch parseFunctionBlock(in: typename, kind: kind, isOverloaded: isOverloaded) {
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
    // MARK: - Statement Parser Common Procs
    /// 単文暗黙ブロックの解析
    func parseImplicitSingleStatementBlock(token: Token, precedence: Precedence?, stopWhen: (Expression, [Expression]) -> Bool) -> BlockStatement? {
        openBlock(kind: .implicit, with: token)
        guard let stmt = parseExpressionStatement(token: token, precedence: precedence, stopWhen: stopWhen) else {
            return nil
        }
        guard closeBlock(kind: .implicit) else {
            return nil
        }
        return BlockStatement(token: token, statements: [stmt])
    }
    /// 式文の解析
    func parseExpressionStatement(token: Token, precedence: Precedence?, stopWhen: (Expression, [Expression]) -> Bool) -> Statement? {
        var expressions: [Expression] = []
        var semanticStartIndex = 0
        while !currentToken.isEndOfStatement {
            guard let expression = ExpressionParser(parser).parse() else {
                return nil
            }
            expressions.append(expression)
            // Existing LHS semantics
            if expression.isAssignment {
                splitElementAssignmentTarget(in: &expressions, from: semanticStartIndex)
                expressions.markLhsCandidates(from: semanticStartIndex)
                semanticStartIndex = expressions.count
            } else if isLhsAssignPredicate(expression) {
                expressions.markLhsCandidates(from: semanticStartIndex)
                semanticStartIndex = expressions.count
            }
            _ = getNext(whenNextIs: .COMMA)     // 読点を読み飛ばし、
            // Stop on next end-of-statement signals or stopWhen condition
            if getNextWhenNextIsEndOfStatement || currentToken.isBreakFactor { break }
            if stopWhen(expression, expressions) { break }
            getNext()                           // 次の式解析に
        }
        let terminator = SentenceTerminator(symbol: currentToken.literal)
        if previousToken.isComma && (terminator == .period || terminator == .rbbracket) {
            error(message: "文の終端前の読点「、」が正しくありません。")
            return nil
        }
        let es = ExpressionStatement(token: token, expressions: expressions, terminator: terminator)
        if parser.options.useSentenceAST {
            return ExpressionStatementParser(parser).parseSentecne(from: es)
        }
        return es
    }
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
    /// 要素代入する対象の属格を２つの句に分割する
    private func splitElementAssignmentTarget(
        in exps: inout [Expression],
        from startIndex: Int
    ) {
        // 分割候補
        var candidate: (index: Int, left: PhraseExpression, right: PhraseExpression)? = nil
        for i in startIndex..<exps.count {
            if candidate != nil,
               let predicate = exps[i] as? PredicateExpression {
                if predicate.isAssignment {
                    break
                } else {
                    candidate = nil // 分割候補をキャンセル
                }
            }
            guard let genitive = exps[i] as? GenitiveExpression,
                  let rightPhrase = genitive.right as? PhraseExpression,
                  rightPhrase.hasParticle(.NI)  else {
                continue
            }
            // 分割候補を設定
            candidate = (i, PhraseExpression(token: Token(.NO), left: genitive.left), rightPhrase)
        }
        // 属格を句に分割
        if let candidate {
            exps[candidate.index] = candidate.left
            exps.insert(candidate.right, at: candidate.index + 1)
        }
    }
    // MARK: - Validate AST
    /// 右辺チェック
    func validateRhsType(from stmt: Statement) -> Bool {
        guard
            stmt is SimpleSentence ||
            stmt is CompoundStatement ||
            stmt is ExpressionStatement
        else {
            error(message: "右辺値に、「\(stmt.string.withoutPeriod)」は使えません。")
            return false
        }
        if let cs = stmt as? CompoundStatement {
            for s in cs.sentences {
                guard validateRhsType(from: s) else { return false }
            }
        }
        return true
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
enum ParserError : Error {
    case unmatchedClosingBlacket
    case mismatchedBlockClose
    case blockParseError
    var message: String {
        switch self {
        case .unmatchedClosingBlacket:
            return "ブロック記号が一致しません。"
        case .mismatchedBlockClose:
            return "ブロック記号以外でブロックが終了しています。"
        case .blockParseError:
            return "ブロックの解析に失敗しました。"
        }
    }
}
/// 式文要素の左辺候補の操作を行う。
extension Array where Element == Expression {
    /// 文中に仮の左辺候補としてマークされた識別子を正式な左辺として確定する
    func finalizeLhsCandidates() {
        self
            .compactMap { $0 as? PhraseExpression }
            .compactMap { $0.left as? Identifier }
            .filter { $0.isLhsCandidate }
            .forEach {
                $0.isLhs = true
                $0.isLhsCandidate = false
            }
    }
    /// 代入先の識別子を抽出
    func extractLhsIdentifier() -> Identifier? {
        self
            .compactMap { $0 as? PhraseExpression }
            .compactMap { $0.left as? Identifier }
            .first { $0.isLhsCandidate }
    }
    /// 代入先が不変(immutable)かをチェック
    /// - Returns:
    ///     1. 「aのbに」で、aが識別子でない場合、true (immutable)
    ///     2. 「aに」で、aが識別子でない場合、true (immutable)
    var hasImmutableLhs: Bool {
        for (i, element) in self.enumerated() {
            // 〜に
            guard let lhsPhrase = element as? PhraseExpression,
                  lhsPhrase.hasParticle(.NI) else {
                continue
            }
            // 〜の〜に
            if i > 0, let genitivePhrase = self[i-1] as? PhraseExpression,
               genitivePhrase.hasParticle(.NO) {
                return !(genitivePhrase.left is Identifier)
            }
            return !(lhsPhrase.left is Identifier)
        }
        return false
    }
    /// LHSフラグ付け
    /// 1. <識別子>に
    /// 2. <識別子>の<式>に (式の型に依らない)
    /// 3. <識別子>と<識別子>に
    enum LhsCandidate {
        case TO(Identifier)
        case NO(Identifier)
        case NI(Identifier?)
        //
        init?(from phrase: PhraseExpression) {
            switch phrase.token {
            case Token(.TO) where phrase.left is Identifier:
                self = .TO(phrase.left as! Identifier)
            case Token(.NO) where phrase.left is Identifier:
                self = .NO(phrase.left as! Identifier)
            case Token(.NI):    // 識別子でない場合(nil)も無効なLHS候補とする
                self = .NI(phrase.left as? Identifier)
            default:
                return nil
            }
        }
        // 対象の(有効な)<識別子>を候補としてマークする
        func mark() {
            switch self {
            case .TO(let id),.NO(let id),.NI(let id?):
                id.isLhsCandidate = true
            default:
                return
            }
        }
    }
    mutating func markLhsCandidates(from startIndex: Int) {
        var candidates: [LhsCandidate] = []
        for exp in self[startIndex...] {
            // 文中に関係の無い述語があれば、その前の候補をキャンセル
            if let predicate = exp as? PredicateExpression,
               !predicate.token.hasLhsIdentifier {
                candidates.removeAll()
                continue
            }
            // 候補対象の絞り込み
            guard let phrase = exp as? PhraseExpression,
                  let candidate = LhsCandidate(from: phrase) else {
                continue
            }
            switch candidate {
            case .TO:
                candidates.append(candidate)
            case .NO:
                candidates = [candidate]
            case .NI:
                if case .NO = candidates.first {
                    break                       // 「<識別子>に」を候補としない
                } else {
                    candidates.append(candidate)
                }
            }
        }
        // 確定した候補の識別子のフラグをオンにする。
        candidates.forEach { $0.mark() }
    }
    /// 識別子候補を破棄する。
    mutating func clearLhsCandidates() {
        for i in indices {
            if let phrase = self[i] as? PhraseExpression,
               let ident = phrase.left as? Identifier {
                ident.isLhsCandidate = false
            }
        }
    }
}
extension Token {
    var isEndOfStatement: Bool {
        isSymbol(.PERIOD) || isSymbol(.EOL) || isSymbol(.EOF) || isSymbol(.RBBRACKET)
    }
    var isBreakFactor: Bool {isEndOfStatement}
    var isEndOfElements: Bool {isComma || isBreakFactor}
    var isEndOfBlock: Bool {isSymbol(.RBBRACKET)}
    func isEndOfBlock(kind: BlockKind) -> Bool {
        switch kind {
        case .explicit:
            return isSymbol(.RBBRACKET)
        case .implicit:
            return isSymbol(.PERIOD) || isSymbol(.EOL)
        }
    }
}
//
// MARK: - statemt parsers and those instance factory
struct StatementParserFactory {
    static func create(from parser: Parser) -> StatementParsable {
        if parser.currentToken.isIdent {
            switch parser.nextToken {
            case .particle(.WA):
                return DefineStatementParser(parser)
            case .particle(.TOWA):
                return ComputaitionDefinitionStatementParser(parser)
            default:
                break
            }
        }
        return ExpressionStatementParser(parser)
    }
}
/// - 形式：<識別子>は、<式(値)>。
struct DefineStatementParser : StatementParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Statement? {
        let identifier = Identifier(from: currentToken.literal)
        parser.insert(identifier.value)     // 識別子をLexerに登録
        getNext()
        let token = currentToken            // 「は」
        _ = getNext(whenNextIs: .COMMA)     // 読点(、)を読み飛ばす
        let isExtended = getNext(whenNextIs: DefineStatement.further)   // 「さらに、」
        _ = getNext(whenNextIs: .COMMA)
        getNext()
        guard let parsed = ExpressionStatementParser(parser).parse() else {
            error(message: "定義文「\(identifier.value)は、<式(値)>。」で、式の解釈に失敗しました。")
            return nil
        }
        if !currentToken.isEndOfBlock {
            _ = getNext(whenNextIs: .PERIOD)
            skipNextEols()                  // EOLの前で解析を停止する。
        }
        if let function = parsed.literal as? FunctionLiteral {
            function.name = identifier.value// 関数の名前を記録
            function.function.isOverloaded = isExtended
        }
        guard validateRhsType(from: parsed) else { return nil }
        return DefineStatement(token: token, name: identifier, value: parsed, isExtended: isExtended)
    }
}
    /// - 形式：<識別子>とは、<算出(取得)の定義>。
    struct ComputaitionDefinitionStatementParser : StatementParsable {
        init(_ parser: Parser) {self.parser = parser}
        let parser: Parser
        func parse() -> Statement? {
            let identifier = Identifier(from: currentToken.literal)
            parser.insert(identifier.value)     // 識別子をLexerに登録
            getNext()
            let token = currentToken            // 「とは、」
            _ = getNext(whenNextIs: .COMMA)
            let isExtended = getNext(whenNextIs: DefineStatement.further)   // 「さらに、」
            _ = getNext(whenNextIs: .COMMA)
            // 算出(取得)の定義
            let header = Token(keyword: .COMPUTATION)
            let getters = FunctionBlocks()
            let setters = FunctionBlocks()
            let kind = BlockKind(isExplicit: getNext(whenNextIs: .LBBRACKET))
            if kind == .implicit,
               case .keyword(let keyword) = nextToken,
               keyword == .COMPUTATION {
                print("""
                    警告：算出定義文内で「算出」が指定されています。これは「算出を返す算出」として解釈されます。
                    　　　通常は「算出」は不要です。
                """)
            }
            switch parseFunctionBlock(in: header.literal, kind: kind) {
            case .success(let function):
                _ = getters.append(function)
            case .failure(let e):
                error(message: "算出の解析に失敗しました。\(e.message)")
                return nil
            }
            /* 「こと」はLexerによってミュートされるため、以下は機能しない。 */
            _ = getNext(whenNextIs: DefineStatement.koto)   // 「こと。」
            _ = getNext(whenNextIs: .PERIOD)
            //
            let cl = ComputationLiteral(token: header, setters: setters, getters: getters)
            let es = ExpressionStatement(token: header, expressions: [cl])
            return DefineStatement(token: token, name: identifier, value: es, isExtended: isExtended)
        }
    }
/// 文の終わりまで、式を解析する。
/// 文の終わり：句点、または改行
/// 解析停止：EOF、ブロックの終わり(】)
struct ExpressionStatementParser : StatementParsable {
    init(_ parser: Parser, until token: Token? = nil) {self.parser = parser; self.endToken = token}
    let parser: Parser, endToken: Token?
    func parse() -> Statement? {
        let token = currentToken
        let stmt = parseExpressionStatement(token: token, precedence: nil) { _, _ in
            self.nextToken == self.endToken
        }
        return stmt
    }
}
/// ブロック(【】)内で式を解析したstatementを、statementsに格納
/// 解析停止：EOF、ブロックの終わり
struct BlockStatementParser : StatementParsable {
    init(_ parser: Parser, kind: BlockKind) {self.parser = parser;self.kind = kind}
    let parser: Parser, kind: BlockKind
    func parse() -> Statement? {
        parser.switchCase.enter()
        var blockStatements: [Statement] = []
        getNext()
        let token = currentToken
        openBlock(kind: kind, with: token)
        let inBlock = kind == .explicit
        while !isEndOfBlock(inBlock) && !currentToken.isEof {
            skipEols(inBlock)                    // ブロック内での改行は読み飛ばす
            guard let statement = StatementParserFactory.create(from: parser).parse() else {
                error(message: "ブロック(【】)内で文の解析に失敗しました。")
                return nil
            }
            blockStatements.append(statement)
            if currentToken.isEof {break}
            if currentToken.isEndOfBlock && shouldCloseExplicitBlock() {break}
            if currentToken.isEndOfStatement && shouldCloseImplicitBlock() {break}
            getNext()                                       // 句点等を読み飛ばす。
        }
        guard closeBlock(isExplicit: inBlock) else {
            return nil
        }
        if parser.switchCase.isActive {
            parser.errors.append(parser.switchCase.defaultError)
            return nil
        }
        parser.switchCase.leave()
        return BlockStatement(token: token, statements: blockStatements)
    }
    var blockStatement: BlockStatement? {parse() as? BlockStatement}
    private func isEndOfBlock(_ isInBlock: Bool) -> Bool {
        if isInBlock && currentToken.isEndOfBlock {return true}
        if !isInBlock && currentToken.isSymbol(.PERIOD) {
            getNext()
            return true
        }
        return false
    }
    /// 【】ブロック内の改行は読み飛ばす。
    private func skipEols(_ isInBlock: Bool) {
        guard isInBlock else {return}
        skipEols()
    }
}
// MARK: - expression parser
/// 中間置演算子の優先順位
enum Precedence : Int {
    case lowest = 1, block, genitive, or, and
    static func < (lhs: Self, rhs: Self) -> Bool {lhs.rawValue < rhs.rawValue}
    static func > (lhs: Self, rhs: Self) -> Bool {lhs.rawValue > rhs.rawValue}
    static func >= (lhs: Self, rhs: Self) -> Bool {lhs.rawValue >= rhs.rawValue}
    static let precedences: [Token.TokenType: Self] = [
        .keyword(.AND):         .and,
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
            error(message: "式の解析で、「\(currentToken)」に対応する構文解析方法が実装されていません。")
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
        case .keyword(.IDENTIFIER),.keyword(.FILE),.keyword(.MEMBER),.keyword(.OUTER):
                                    return LabelExpressionParser(parser)
        case .keyword(.ITS):        return PropertyExpressionParser(parser)
        case .keyword(.ELSE):
            parser.errors.append("場合文外で、「それ以外」が使用されています。")
            return nil
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
        // 中黒「・」で分離された列挙子
        let names = token.literal.components(separatedBy: EnumeratorLiteral.dot)
        if names.count == 2 {
            let enumerator = EnumeratorLiteral(token: token, type: names[0], name: names[1])
            return parseRangeExpression(with: enumerator)
        }
        // 範囲への変換を試みる(例：<識別子>以上)
        return parseRangeExpression(
            with: Identifier(
                from: token,
                with: parseAuxiliaryToken()
            )
        )
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
            error(message: "整数リテラルの解析で、「\(currentToken.literal)」を整数に変換できません。")
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
        let label = currentToken                // ラベル
        if label.isKeyword(.OUTER) {            // 外部(outer)識別子
            getNext()
            return Identifier(
                from: currentToken,
                with: parseAuxiliaryToken(),
                isOuter: true
            )
        }
        switch nextToken.type {
        case .string,.ident,.int,.keyword(.TRUE),.keyword(.FALSE):
            break
        default:
            error(message: "「\(label.literal)」の後続が<文字列>、<識別子>、<真>、<偽>または<数値>ではありません。)")
            return nil
        }
        getNext()
        return Label(token: label, value: currentToken)
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
        let kind = BlockKind(isExplicit: getNext(whenNextIs: .LBBRACKET))
        guard let block = BlockStatementParser(parser, kind: kind).blockStatement else {
            error(message: "範囲で、範囲式の解析に失敗しました。")
            return nil
        }
        guard block.statements.count == 1,
              let es = block.statements.first as? ExpressionStatement else {
            error(message: "範囲で、範囲式の解析に失敗しました。(式が取り出せません。)")
            return nil
        }
        if !parser.options.useSentenceAST {
            return parseRangeExpressions(es.expressions, token: token)
        }

        let expressions = es.expressions
        // Prefer pre-parsed RangeLiteral(s) produced by parseRangeExpression in prefix parsers.
        if let brideged = bridgedRange(in: expressions, headerToken: token) {
            return brideged
        }
        // 下限範囲
        var lowerBoundary: BoundaryExpression? = nil
        var restExpressions: [Expression] = expressions
        if let (lowerToken, lowerIndex) = findBoundary(in: expressions, with: {$0.isLower})  {
            var lowerExprs = expressions.prefix(lowerIndex)
            if let exp = (expressions[lowerIndex] as? PhraseExpression)?.left {
                lowerExprs.append(exp)
            }
            if lowerExprs.isEmpty {
                error(message: "範囲で、下限の式が見つかりません。")
                return nil
            }
            guard let sentence = buildSentence(from: Array(lowerExprs)) else {
                error(message: "範囲で、下限の文の構築に失敗しました。")
                return nil
            }
            lowerBoundary = BoundaryExpression(token: lowerToken, sentence: sentence)
            // Determine rest expressions after lower marker: if marker is PhraseExpression skip one element, else keep from marker
            restExpressions = Array(expressions[(lowerIndex+1)...])
        }
        // 上限範囲
        var upperBoundary: BoundaryExpression? = nil
        if let (upperToken, upperIndex) = findBoundary(in: restExpressions, with: {$0.isUpper})  {
            var upperExprs = restExpressions.prefix(upperIndex)
            if let exp = (restExpressions[upperIndex] as? PhraseExpression)?.left {
                upperExprs.append(exp)
            }
            if upperExprs.isEmpty {
                error(message: "範囲で、上限の式が見つかりません。")
                return nil
            }
            // Check no trailing expressions after upper boundary phrase
            if upperIndex + 1 < restExpressions.count {
                error(message: "範囲で、上限の形式が間違っています。")
                return nil
            }
            guard let sentence = buildSentence(from: Array(upperExprs)) else {
                error(message: "範囲で、上限の文の構築に失敗しました。")
                return nil
            }
            upperBoundary = BoundaryExpression(token: upperToken, sentence: sentence)
        } else {
            // No upper boundary found, restExpressions must be empty
            if !restExpressions.isEmpty {
                error(message: "範囲で、上限の形式が間違っています。")
                return nil
            }
        }
        // If at least one boundary was built, return RangeLiteral with boundaries
        if lowerBoundary != nil || upperBoundary != nil {
            return RangeLiteral(token: token, lower: lowerBoundary, upper: upperBoundary)
        }
        // Otherwise fall back to old behavior
        return parseRangeExpressions(es.expressions, token: token)
    }
    private func bridgedRange(in exprs: [Expression], headerToken: Token) -> RangeLiteral? {
        // Prefer pre-parsed RangeLiteral(s) produced by parseRangeExpression in prefix parsers.
        // 1 item: single boundary (lower or upper)
        if exprs.count == 1, let legacy = exprs.first as? RangeLiteral {
            if let bridged = rangeLiteral(from: legacy, token: headerToken) {
                return bridged
            }
        }
        // 2 items: combination of lower-boundary then upper-boundary (both pre-parsed)
        if exprs.count == 2,
           let firstLegacy = exprs.first as? RangeLiteral,
           let secondLegacy = exprs.last  as? RangeLiteral,
           let firstBridged  = rangeLiteral(from: firstLegacy,  token: headerToken),
           let secondBridged = rangeLiteral(from: secondLegacy, token: headerToken) {
            // Accept only (lower-only) + (upper-only) in this order
            if firstBridged.upperBoundary == nil,
               secondBridged.lowerBoundary == nil {
                return RangeLiteral(
                    token: headerToken,
                    lower: firstBridged.lowerBoundary,
                    upper: secondBridged.upperBoundary
                )
            }
        }
        return nil
    }
    private func findBoundary(in exprs: [Expression], with isBoundary: (Token) -> Bool) -> (Token, Int)? {
        var token: Token?
        guard let i = exprs.firstIndex(where: { expr in
            if let p = expr as? PhraseExpression, isBoundary(p.token) {
                token = p.token
                return true
            }
            return false
        }) else { return nil }
        
        return (token!, i)
    }
    private func rangeLiteral(from legacyRange: RangeLiteral, token: Token) -> RangeLiteral? {
        // Case: already parsed as range by prefix parsers (e.g., "1以上", "xまで").
        // Convert legacy RangeLiteral (lowerBound/upperBound) into boundary-based RangeLiteral if possible.
        if let lb = legacyRange.lowerBound,
           let sentence = buildSentence(from: lb.expressions) {
            let lower = BoundaryExpression(token: lb.token, sentence: sentence)
            var upper: BoundaryExpression? = nil
            if let ub = legacyRange.upperBound,
               let us = buildSentence(from: ub.expressions) {
                upper = BoundaryExpression(token: ub.token, sentence: us)
            }
            return RangeLiteral(token: token, lower: lower, upper: upper)
        }
        if let ub = legacyRange.upperBound,
           let sentence = buildSentence(from: ub.expressions) {
            let upper = BoundaryExpression(token: ub.token, sentence: sentence)
            return RangeLiteral(token: token, lower: nil, upper: upper)
        }
        return nil
    }
    private func buildSentence(from expressions: [Expression]) -> Sentence? {
        guard let last = expressions.last else { return nil }
        if last.isPredicate {
            return SimpleSentence(
                token: last.sentenceToken,
                auxiliaryVerb: last.auxiliaryVerb,
                arguments: expressions.dropLast(),
                predicateKind: last.sentenceToken.isPredicate ? .builtin : .custom,
                string: expressions.map { $0.string }.joined()
            )
        }
        return ExpressionStatement(token: last.token, expressions: expressions)
    }
}
struct FunctionLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = parseHeader()
        let kind = BlockKind(isExplicit: getNext(whenNextIs: .LBBRACKET))
        switch parseFunctionBlock(in: token.literal, kind: kind) {
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
        let kind = BlockKind(isExplicit: getNext(whenNextIs: .LBBRACKET))
        openBlock(kind: kind, with: token)
        skipNextEols(blockKind: kind)
        // Setter block
        var setters = FunctionBlocks()
        switch parseFunctionBlocks(of: ComputationLiteral.settei, in: token.literal) {
        case .success(let blocks):
            setters = blocks
            _ = getNext(whenNextIs: .PERIOD)        // 設定ブロックの句点を飛ばす
            skipNextEols(blockKind: kind)
        case .failure(let e):
            error(message: "算出で、「設定が、〜」の解析に失敗しました。\(e.message)")
            return nil
        }
        // Getter block
        var getters = FunctionBlocks()
        if !isEndOfBlock(kind: kind) {
            switch parseFunctionBlocks(of: ComputationLiteral.syutoku, in: token.literal) {
            case .success(let blocks):
                getters = blocks
                _ = getNext(whenNextIs: .PERIOD)    // 取得ブロックの句点を飛ばす
                skipNextEols(blockKind: kind)
            case .failure(let e):
                error(message: "算出で、「取得が、〜」の解析に失敗しました。\(e.message)")
                return nil
            }
            if getters.isEmpty {                    // 「取得は、」が無かった
                switch parseFunctionBlock(in: token.literal, kind: kind) {
                case .success(let function):
                    _ = getters.append(function)
                case .failure(let e):
                    error(message: "算出の解析に失敗しました。\(e.message)")
                    return nil
                }
            }
        }
        guard closeBlock(kind: kind) else {
            return nil
        }
        if !setters.isEmpty {_ = getNext(kind: kind)}   // 算出ブロックの終わりを読み飛ばす。
        _ = getNext(whenNextIs: .PERIOD)            // 算出ブロックの句点を飛ばす
        skipNextEols(blockKind: kind)
        return ComputationLiteral(token: token, setters: setters, getters: getters)
    }
}
struct ProtocolLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = parseHeader()
        let kind = BlockKind(isExplicit: getNext(whenNextIs: .LBBRACKET))
        // Protocols block
        guard let protocols = parseProtocols() else {return nil}
        // Clauses block
        guard let clauses = parseClauses(kind: kind) else {
            error(message: "規約で、「条項が、〜」の解析に失敗しました。")
            return nil
        }
        _ = getNext(kind: kind)
        return ProtocolLiteral(token: token, protocols: protocols, clauses: clauses)
    }
    /// 条項群の解析
    private func parseClauses(kind: BlockKind) -> [ClauseLiteral]? {
        var clauses: [ClauseLiteral] = []
        _ = getNext(whenNextKeywordIs: ClauseLiteral.joukou)    // 条項が、(条項は、)
    
        openBlock(kind: kind, with: ProtocolLiteral.kiyaku)
        while !nextToken.isEndOfBlock(kind: kind) && !nextToken.isEof {
            guard let clauseGroup = parseClauseGroup(kind: kind) else {return nil}
            clauses.append(contentsOf: clauseGroup)
        }
        guard closeBlock(kind: kind) else {
            return nil
        }
        return clauses
    }
    /// 各条項の解析
    /// 1. (型の)<識別子>は、<型>
    /// 2. (型の)<識別子>は、関数【<入力定義>。<出力定義>】
    /// 3. (型の)<識別子>は、算出【<入力定義>。<出力定義>】
    /// 4. 初期化は(が)、【<入力定義>。<出力定義>】
    private func parseClauseGroup(kind: BlockKind) -> [ClauseLiteral]? {
        skipNextEols(blockKind: kind)
        //
        let isStatic = getNext(whenNextIs: TypeLiteral.katano)  // 型の<識別子>
        var token = nextToken
        var ident = Identifier(from: token)
        switch token {
        case .IDENT:
            getNext()
            ident = Identifier(from: currentToken)              // <識別子>は、
            if isStatic && ident.value == "要素" {
                error(message: "規約で、「型の要素は、【〜】」の定義はできません。個別に「型の<識別子>は、〜」の定義が必要です。")
                return nil
            }
            guard getNext(whenNextIs: .WA) else {
                error(message: "規約で、条項「<識別子>は」の解析に失敗しました。")
                return nil
            }
            _ = getNext(whenNextIs: .COMMA)
            _ = getNext(whenNextIs: DefineStatement.further)    // 「さらに」は読み飛ばす
            _ = getNext(whenNextIs: .COMMA)
            getNext()
            token = currentToken
        case .keyword(.INITIALIZATION):
            if isStatic {
                error(message: "型の初期化はできません(「型の」は不要)。")
                return nil
            }
        default:
            error(message: "規約で、条項(トークン：\(token.literal))の解析に失敗しました。")
            return nil
        }
        guard let signatureKinds = parseClauseContents(from: token) else {return nil}
        _ = getNext(whenNextIs: .PERIOD)
        skipNextEols(blockKind: kind)
        
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
                error(message: "規約で、関数定義の解析に失敗しました。")
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
                error(message: "規約で、算出定義の解析に失敗しました。")
                return nil
            }
            return makeComputationSignatures(from: parsed)
        case .keyword(.INITIALIZATION): // 初期化
            switch parseFunctionBlocks(of: token.literal, in: token.literal) {
            case .success(let blocks):
                return blocks.all.map {
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
            error(message: "規約で、条項の解析に失敗しました。(未対応の型: \(token.literal))")
            return nil
        }
    }
    /// 算出内の多重定義(FunctionBlocks) → [SignatureKind]変換
    private func makeComputationSignatures(from literal: ComputationLiteral) -> [SignatureKind]? {
        var getters = literal.getters.all.map {Optional($0)}
        var setters = literal.setters.all.map {Optional($0)}
        let n = max(getters.count, setters.count)
        guard n > 0 else {
            error(message: "規約の算出には、取得か設定の少なくともいずれかの定義が必要です。")
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
        let kind = BlockKind(isExplicit: getNext(whenNextIs: .LBBRACKET))
        skipNextEols(blockKind: kind)
        // Protocols block
        guard let protocols = parseProtocols() else {return nil}
        skipNextEols(blockKind: kind)
        // Type member block
        var members: BlockStatement?
        switch parseOptionalBlock(of: TypeLiteral.typemembers, in: token.literal) { // 「型の要素は、【<要素定義>】
        case .success(let block):
            members = block
            skipNextEols(blockKind: kind)
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
            skipNextEols(blockKind: kind)
        case .failure(_):
            return nil
        }
        // Body block
        switch parseBlock(name: ExpressionStatement.hontai, kind: kind, in: token.literal) {
        case .success(let body):
            return TypeLiteral(token: token, protocols: protocols, typeMembers: members, initializers: initializers, body: body)
        case .failure(_):
            return nil
        }
    }
    /// 型の<識別子>は、<定義>を型の要素に取り込む
    private func parseTypeMembers(with typeMembers: inout BlockStatement?) -> Bool {
        var definitions = [DefineStatement]()
        while getNext(whenNextIs: TypeLiteral.katano) {
            getNext()   // DefStatemntParserは、currentTokenで処理をするため、1つ進める
            guard let definition = DefineStatementParser(parser).parse() as? DefineStatement else {
                error(message: "型で、「型の<識別子>は、〜」の解析に失敗しました。")
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
        let kind = BlockKind(isExplicit: getNext(whenNextIs: .LBBRACKET))
        openBlock(kind: kind, with: token)
        // 要素の解析
        guard let elements: [Statement] = parseElements(in: token, kind: kind) else {
            error(message: "列挙で、「要素が、〜」の解析に失敗しました。")
            return nil
        }
        guard closeBlock(kind: kind) else {
            return nil
        }
        guard parseEndOfElements(kind: kind) else {return nil}
        return EnumLiteral(token: token, elements: elements)
    }
}
struct ArrayLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = parseHeader()
        let kind = BlockKind(isExplicit: getNext(whenNextIs: .LBBRACKET))
        openBlock(kind: kind, with: token)
        // 要素の解析
        guard let elements: [ExpressionStatement] = parseElements(in: token, kind: kind) else {
            error(message: "配列で、「要素が、〜」の解析に失敗しました。")
            return nil
        }
        guard closeBlock(kind: kind) else {
            return nil
        }
        guard parseEndOfElements(kind: kind) else {return nil}
        return ArrayLiteral(token: token, elements: elements)
    }
}
struct DictionaryLiteralParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = parseHeader()
        let kind = BlockKind(isExplicit: getNext(whenNextIs: .LBBRACKET))
        openBlock(kind: kind, with: token)
        // 要素の解析
        guard let pairs: [PairExpression] = parseElements(in: token, kind: kind) else {
            error(message: "辞書で、「要素が、〜」の解析に失敗しました。")
            return nil
        }
        guard closeBlock(kind: kind) else {
            return nil
        }
        guard parseEndOfElements(kind: kind) else {return nil}
        return DictionaryLiteral(token: token, pairs: pairs)
    }
}
struct PredicateExpressionParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        return PredicateExpression(
            token:          currentToken,
            auxiliaryToken: parseAuxiliaryToken()
        )
    }
}
struct PropertyExpressionParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = currentToken
        guard nextToken.isIdent || nextToken.isKeyword(.TYPE) else {
            error(message: "「\(nextToken.literal)」は属性名ではありません。")
            return nil
        }
        getNext()
        return PropertyExpression(token: token, property: currentToken)
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
        guard let consequence = parseConsequenceBlock() else {
            error(message: "「場合、」に続くブロック解析に失敗しました。")
            return nil
        }
        _ = getNext(whenNextIs: .COMMA)                     // 読点(、)を読み飛ばす
        _ = getNext(whenNextIs: .EOL)                       // EOLを読み飛ばす
        var alternative: BlockStatement? = nil
        if getNext(whenNextIs: CaseExpression.soreigai) {   // それ以外
            _ = getNext(whenNextIs: ExpressionStatement.wa) // (は)
            _ = getNext(whenNextIs: .COMMA)                 // (、)
            let kind = BlockKind(isExplicit: getNext(whenNextIs: .LBBRACKET))
            alternative = BlockStatementParser(parser, kind: kind).blockStatement
            if alternative == nil {
                error(message: "場合文の「それ以外」に続くブロック解析に失敗しました。")
                return nil
            }
        }
        return CaseExpression(token: token, consequence: consequence, alternative: alternative)
    }
    /// 「場合」に続く文を解析する。
    private func parseConsequenceBlock() -> BlockStatement? {
        if getNext(whenNextIs: .LBBRACKET) {
            return BlockStatementParser(parser, kind: .explicit).blockStatement
        }
        // Non-block: parse a single ExpressionStatement up to period/EOL or before "それ以外は"
        getNext()
        let token = currentToken
        return parseImplicitSingleStatementBlock(token: token, precedence: nil) { _, _ in
            self.lookaheadIsElse()
        }
    }
    private func lookaheadIsElse() -> Bool {
        // Non-tokenized phrase detection: check upcoming stream for "それ以外は" optionally followed by a comma
        let temp = Parser(from: parser)
        // Try to match the exact literal first; rely on existing getNext(whenNextIs:) overload for String
        if temp.getNext(whenNextIs: CaseExpression.soreigai) {   // それ以外
            _ = temp.getNext(whenNextIs: ExpressionStatement.wa) // (は)
            _ = temp.getNext(whenNextIs: .COMMA)
            return true
        }
        return false
    }
}
/// 「または」または「かつ」で始まる式を解析し、続くブロック(または条件式)をLogicalExpression.rightとする。
struct LogicalExpressionParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> Expression? {
        let token = currentToken                            // 「または」または「かつ」
        let precedence = Precedence[token.type]
        _ = getNext(whenNextIs: .COMMA)                     // (、)
        guard let consequence = parseConditionalBlock(precedance: precedence) else {
            error(message: "「\(currentToken.literal)、」に続くブロック解析に失敗しました。")
            return nil
        }
        _ = getNext(whenNextIs: .COMMA)                     // (、)のみ読み飛ばす
        return LogicalExpression(token: token, right: consequence)
    }
    ///  論理式の右辺(条件式)を解析する
    private func parseConditionalBlock(precedance: Precedence) -> BlockStatement? {
        if getNext(whenNextIs: .LBBRACKET) {
            return BlockStatementParser(parser, kind: .explicit).blockStatement
        }
        getNext()
        let token = currentToken
        return parseImplicitSingleStatementBlock(token: token, precedence: precedance) { _, _ in
            currentToken.isBreakFactor || shouldFoldLogicalExpression(currentPrecedence: precedance)
        }
    }
    /// 論理式を畳み込むか否か
    /// 畳み込みにより、かつ > または、の優先順位を実現する。
    /// - Parameter currentPrecedence: 左側の優先順位
    /// - Returns: true: 左結合、false: 右結合
    private func shouldFoldLogicalExpression(currentPrecedence: Precedence) -> Bool {
        // 文・行の終端
        if nextToken.isPeriod || nextToken.isEol {
            return true
        }
        // 次の論理演算子の方が弱い場合は、ここで畳む（左結合）
        if nextToken.consumeLogicalExpression &&
            (currentPrecedence >= Precedence[nextToken.type]) {
            return true
        }
        return false
    }
}
/// 条件演算「よって」
/// <条件>(か)によって、<式１>か<式２>
struct ConditionalOperationParser : ExpressionParsable {
    init(_ parser: Parser) {self.parser = parser}
    let parser: Parser
    func parse() -> (any Expression)? {
        guard previousToken.isParticle(.NI) else {
            error(message: "「よって」の前には助詞「に」が必要です。")
            return nil
        }
        let token = currentToken                            // よって
        _ = getNext(whenNextIs: .COMMA)                     // (、)
        getNext()
        guard let consequence = ExpressionParser(parser).parse() else {
            error(message: "「(か)によって」のに続く式の解析に失敗しました。")
            return nil
        }
        guard getNext(whenNextIs: ConditionalOperation.ka) else {   // か
            error(message: "「(か)によって」の後続に「か」が見つかりません。")
            return nil
        }
        _ = getNext(whenNextIs: .COMMA)                     // (、)
        getNext()
        guard let alternative = ExpressionParser(parser).parse() else {
            error(message: "「(か)によって」の「か」に続く式の解析に失敗しました。")
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
    init(_ parser: Parser) { self.parser = parser }
    let parser: Parser

    func parse() -> Expression? {
        let token = parseHeader()
        let kind = BlockKind(isExplicit: getNext(whenNextIs: .LBBRACKET))
        // Input block
        guard let (identifiers, _) = parseInputBlock(in: token.literal) else { return nil }
        // Condition block
        let condition: Statement?
        do {
            condition = try parseCondition()
        } catch {
            self.error(message: "反復で、「条件が〜の間、」の解析に失敗しました。")
            return nil
        }
        // Body block
        switch parseBlock(name: LoopExpression.syori, kind: kind, in: token.literal) {    // 処理が、(処理は、)
        case .success(let block?):
            return LoopExpression(token: token, parameters: identifiers, condition: condition, body: block)
        case .success(nil):
            error(message: "反復で、処理の解析に失敗しました。(本体が見つりません)")
        default:
            break
        }
        return nil
    }
    enum LoopExpressionError : Error {
        case input, condition, body
    }
    private func parseCondition() throws -> Statement? {
        guard getNext(whenNextKeywordIs: LoopExpression.condition) else { return nil }
        if getNext(whenNextIs: .LBBRACKET) {
            let block = BlockStatementParser(parser, kind: .explicit).blockStatement
            _ = getNext(whenNextIs: LoopExpression.aida)
            _ = getNext(whenNextIs: .COMMA)
            return block
        }
        getNext()
        guard let stmt = ExpressionStatementParser(parser, until: Token(.WHILE)).parse() else { return nil }
        _ = getNext(whenNextIs: LoopExpression.aida)
        _ = getNext(whenNextIs: .COMMA)
        return stmt
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
            error(message: "中間置式(\(op)で、左辺の解析に失敗しました。")
            return nil
        }
        _ = getNext(whenNextIs: .COMMA)
        let precedence = Precedence[currentToken.type]
        getNext()
        guard let right = ExpressionParser(parser, precedence: precedence).parse() else {
            error(message: "中間置式(\(op)で、右辺の解析に失敗しました。")
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
        _ = getNext(whenNextKeywordIs: CallExpression.arguments)   // 引数が(は)、
        guard let caller = left else {
            error(message: "呼び出し式で、左辺の解析に失敗しました。")
            return nil
        }
        guard let block = BlockStatementParser(parser, kind: .explicit).blockStatement else {
            error(message: "呼び出し式で、引数の解析に失敗しました。")
            return nil
        }
        var arguments: [DefineStatement] = []
        for statement in block.statements {
            guard let define = statement as? DefineStatement else {
                error(message: "呼び出し式の引数が定義文で定義されていません。")
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
            error(message: "属格で、左式の解析に失敗しました。")
            return nil
        }
        if nextToken.isPeriod || nextToken.isEol || nextToken.isEof {
            return PhraseExpression(token: currentToken, left: left)
        }
        let precedence = Precedence[currentToken.type]
        getNext()
        guard let right = ExpressionParser(parser, precedence: precedence).parse() else {
            error(message: "属格で、右式の解析に失敗しました。")
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
                guard let parsed = ExpressionStatementParser(parser).parse() else {
                    error(message: "属格で、値式の解釈に失敗しました。")
                    return nil
                }
                guard validateRhsType(from: parsed) else { return nil }
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
            error(message: "「\(token.literal)」格の左辺(式)の解析に失敗しました。")
            return nil
        }
        return PhraseExpression(token: token, left: left)
    }
}

