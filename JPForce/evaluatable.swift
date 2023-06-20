//
//  evaluatable.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/03/06.
//

import Foundation

// MARK: - intefaces
protocol Evaluatable {
    /// ASTノードの評価を行い、結果のオブジェクトを返す。
    /// - Returns: 返すオブジェクトが無い場合、nilを返す。エラーの場合は、JpfErrorを返す。
    func evaluated(with environment: Environment) -> JpfObject?
}
// MARK: - implements (evalutable extentions)
// MARK: 文と式を評価して、オブジェクト(JpfObject)を生成
extension Node {
    func evaluated(with environment: Environment) -> JpfObject? {
        return (self.string + "(\(type(of: self)))" + notImplementedError)
    }
    // 評価エラー
    var notImplementedError: JpfError       {JpfError("の評価を未実装")}
    var phaseValueNotFound: JpfError        {JpfError("句の値が無かった。(例：関数の返り値が無い。)")}
    var predicateNotSupported: JpfError     {JpfError("「述語」に対応する定義が見つからなかった。")}
    var identifierNotFound: JpfError        {JpfError("(識別子)が定義されていない。")}
    var caseConditionError: JpfError        {JpfError("「場合」の条件(真偽値)が見つからなかった。")}
    var logicalConditionError: JpfError     {JpfError("の前に条件(真偽値)が見つからなかった。")}
    var logicalRightEvaluationError: JpfError   {JpfError("の後に条件(真偽値)の評価に失敗した。")}
    var logicalOperatorError: JpfError      {JpfError("は、論理式の演算に使えない。")}
    var elementEvalError: JpfError          {JpfError("「配列」の要素の評価に失敗した。要素：")}
    var unusableAsHashKey: JpfError         {JpfError("「辞書」の要素の索引が、ハッシュキーとして使用できない。索引: ")}
    var keywordNotSupportedInInfixExpression: JpfError   {JpfError("は、値の選択に使用できない。仕様：<値>または<値>...")}
    var conditionEvaluationError: JpfError  {JpfError("「反復」の条件が正しくない。")}
    var loopParameterError: JpfError        {JpfError("「反復」の入力が正しくない。")}
    var rangeTypeError: JpfError            {JpfError("「範囲」の上下限が、数値でない。：")}
    var overloadExtentionError: JpfError    {JpfError("「関数」で拡張している。")}
    var protocolExtentionError: JpfError    {JpfError("「型」以外を「さらに」で拡張している。")}
    var enumuratorError: JpfError           {JpfError("「列挙」の列挙子(識別子)が正しくない。：")}
    // 仕様表示
    var strideLoopUsage: JpfError           {JpfError("仕様：<数値>から<数値>まで（<数値>ずつ）反復【入力が<識別子(カウント値)>、<処理>】。")}
    var rangeLoopUsage: JpfError            {JpfError("仕様：範囲【<下限><上限>】を反復【入力が<識別子(カウント値)>、<処理>】。")}
    var arrayLoopUsage: JpfError            {JpfError("仕様：<配列>を反復【入力が<識別子(値)>、<処理>】。")}
    var dictionaryLoopUsage: JpfError       {JpfError("仕様：<辞書>を反復【入力が<識別子(キー)>と<識別子(値)>、<処理>】。")}
    var whileLoopUsage: JpfError            {JpfError("仕様：反復【条件が<条件式>(の)間、<処理>】。")}
    var closedLoopUsage: JpfError           {JpfError("仕様：反復【<処理>。<条件式>場合、中止する】。")}
}
// MARK: Program/Statement/Block evaluators
extension Program : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        var result: JpfObject?
        for statement in statements {
            result = statement.evaluated(with: environment)
            if let object = result, object.isBreakFactor {return object.value}
        }
        return result
    }
}
extension ExpressionStatement : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        var result: JpfObject?
        for expression in expressions {
            result = expression.evaluated(with: environment)
            if let object = result, object.isBreakFactor {break}
            result.map {environment.push($0)}
        }
        return result
    }
}
extension BlockStatement : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        var result: JpfObject?
        for statement in statements {
            result = statement.evaluated(with: environment)
            if let object = result, object.isBreakFactor {break}
        }
        return result
    }
}
extension DefineStatement : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        if let result = value.evaluated(with: environment), result.isError {return result}
        var object = environment.pull()
        if isExtended {                             // 多重定義
            if let function = object as? JpfFunction {                  // 関数多重定義
                let result = overload(function, with: environment)
                guard !result.isError else {return result}
                object = result
            } else
            if let orignal = environment[name.value] as? JpfProtocol {  // 規約デフォルト実装
                guard let extended = object as? JpfType else {return protocolExtentionError + "(拡張先が\(object?.type ?? "無い"))"}
                object = JpfProtocol(protocols: orignal.protocols, clauses: orignal.clauses, body: extended.body)
            }
        }
        object?.name = name.value
        environment[name.value] = object
        return nil
    }
    private func overload(_ function: JpfObject, with environment: Environment) -> JpfObject {
        var elements: [JpfObject] = []
        if let original = environment[name.value] { // 既存定義
            switch original {
            case let f as JpfFunction:              // 関数定義
                elements.append(f)
            case let a as JpfArray:                 // 多重定義
                elements = a.elements
            default:
                return "拡張元(\(original.type))を" + overloadExtentionError
            }
        }
        elements.append(function)                   // 多重定義(配列)に関数を追加
        return JpfArray(elements: elements)
    }
}
// MARK: Expression evaluators
extension IntegerLiteral : Evaluatable {
    /// 数値オブジェクトを返す。
    /// または、数値で配列もしくは辞書の要素を取り出し返す。
    func evaluated(with environment: Environment) -> JpfObject? {
        accessed(with: environment) ?? JpfInteger(value: value)
    }
}
extension StringLiteral : Evaluatable {
    /// 文字列オブジェクトを返す。
    /// または、文字列で辞書の要素を取り出し返す。
    func evaluated(with environment: Environment) -> JpfObject? {
        accessed(with: environment) ?? JpfString(value: value)
    }
}
extension Boolean : Evaluatable {
    /// 真偽値オブジェクトを返す。
    /// または、真偽値で辞書の要素を取り出し返す。
    func evaluated(with environment: Environment) -> JpfObject? {
        accessed(with: environment) ?? JpfBoolean(value: value)
    }
}
extension RangeLiteral : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        environment.push(JpfNull.object)    // 範囲の上下限が、配列にアクセスしないための回避策
        var lowerBound, upperBound: (JpfInteger, Token)?
        if let object = self.lowerBound?.evaluated(with: environment) {
            if object.isError {return object}
            guard let value = environment.peek as? JpfInteger else {return rangeTypeError + object.string}
            environment.drop()
            lowerBound = (value, self.lowerBound!.token)
        }
        if let object = self.upperBound?.evaluated(with: environment) {
            if object.isError {return object}
            guard let value = environment.peek as? JpfInteger else {return rangeTypeError + object.string}
            environment.drop()
            upperBound = (value, self.upperBound!.token)
        }
        let range = JpfRange(lowerBound: lowerBound, upperBound: upperBound)
        if let o = environment.peek, o.isNull {environment.drop()}
        return accessed(by: range, with: environment) ?? range
    }
}
extension Identifier : Evaluatable {
    /// 識別子をキーに、辞書からオブジェクトを取り出す。(あれば入力に積まれる。)
    /// 入力がノ格のオブジェクトである場合、識別子でsubscriptアクセスをし、値を取り出す。(例：〜の型)
    /// - オブジェクトが実行可能な関数の場合、実行結果(返り値またはエラー)を返す。
    /// - オブジェクトが数値で、入力がノ格の配列の場合、配列要素を取り出して返す。(範囲外は「無」を返す。)
    /// - オブジェクトが索引(ハッシュキー)で、入力がノ格の辞書の場合、値を取り出して返す。
    func evaluated(with environment: Environment) -> JpfObject? {
        guard let object = getObject(from: environment, with: value) else {return "『\(value)』" + identifierNotFound}
        return object.accessed(by: value, with: environment)
    }
    ///　前句のオブジェクトにnameで問い合わせ、結果のオブジェクトを得る。
    ///　または、識別名でオブジェクトを取得（できなければ識別名の終止形のでオブジェクトを取得）
    /// - Parameters:
    ///   - environment: 識別子の辞書、およびスタック
    ///   - name: キーとなる識別名
    /// - Returns: 対応するオブジェクト
    private func getObject(from environment: Environment, with name: String) -> JpfObject? {
        if let object = getEnumerator(from: environment, with: name) {
            return object
        }
        let particle = environment.peek?.particle
        if let object = environment.unwrappedPeek?[name, particle] { // JpfObjectにsubscriptアクセス
            environment.drop()
            return object
        }
        return environment[name] ?? ContinuativeForm(name).plainForm.flatMap {environment[$0]}
    }
    private func getEnumerator(from environment: Environment, with name: String) -> JpfObject? {
        let dot = "・"
        if name.contains(dot) {
            let strings = name.components(separatedBy: dot)
            if strings.count == 2 {         // (列挙型)・列挙子
                guard let enumObject = getEnumObject(from: environment, with: strings) else {
                    return "『\(strings.first!)』" + identifierNotFound
                }
                return JpfEnumerator(type: enumObject.name, identifier: strings.last!, rawValue: enumObject.environment[strings.last!])
            }
        }
        return nil
    }
    private func getEnumObject(from environment: Environment, with names: [String]) -> JpfEnum? {
        if !names[0].isEmpty, let object = environment[names[0]] as? JpfEnum {return object}
        for value in environment.values {
            if let object = value as? JpfEnum, object.elements.contains(names[1]) {
                return object
            }
        }
        return nil
    }
}
extension PredicateExpression : Evaluatable {
    /// 述語を実行する。(結果があれば返す。= スタックに積まれる。)
    /// self.tokenは述語(Token.Keywordもしくは Token.IDENT(_))
    func evaluated(with environment: Environment) -> JpfObject? {
        guard let predicate = PredicateOperableFactory.create(from: token, with: environment) else {return predicateNotSupported}
        return predicate.operated()
    }
}
extension PhraseExpression : Evaluatable {
    /// 句(式+助詞)を返す。
    /// self.tokenは、助詞(Token.Particle)
    func evaluated(with environment: Environment) -> JpfObject? {
        guard let result = left.evaluated(with: environment) else {return nil}
        // TODO: 関数で値を返さないとnilが返り句もnilになってしまう。(スタックに値が残る)
        // ここでエラーにすると、「空にする」の「に」等、冗長な句もエラーになってしまう。
        guard !result.isError else {return result}
        return JpfPhrase(value: result, particle: token)
    }
}
extension InfixExpression : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        guard token == .keyword(.OR) else {return "「\(tokenLiteral)」" + keywordNotSupportedInInfixExpression}
        var array: [JpfObject] = []
        guard let value = left.evaluated(with: environment) else {return nil}
        guard !value.isError else {return value}
        array.append(value)
        guard let value = right.evaluated(with: environment) else {return nil}
        guard !value.isError else {return value}
        guard let particle = value.particle else {return nil}
        if let arrayObject = value.value as? JpfArray {
            array += arrayObject.elements
        } else {
            array.append(value.value!)
        }
        return JpfPhrase(value: JpfArray(elements: array), particle: particle)
    }
}
extension CaseExpression : Evaluatable {
    /// 条件処理(場合分け)。
    /// 形式１：　(条件)場合、【処理】(、それ以外は、【処理】)
    /// 形式２：   (〜が、〜の)場合、【処理】(、(〜の)場合、【処理】...)(、それ以外は、【処理】)
    /// - Returns: ReturnValueまたはnil、エラー
    func evaluated(with environment: Environment) -> JpfObject? {
        if let condition = getCondition(from: environment) {
            guard !condition.isError else {return condition}
            let result =  condition.isTrue ?
            consequence.evaluated(with: environment) :
            alternative?.evaluated(with: environment)
            if let value = result, value.isBreakFactor {return value}
        }
        return nil
    }
    /// 「場合」の前の条件式を評価し、結果(JpfObject)を返す。
    /// - Parameter environment: 入力を参照し、必要に応じて破棄する。
    /// - Returns: 〜が〜の場合：一致したら真を、そうでなければ偽を返す。入力は、真の場合２つ、偽の場合１つ捨てる。
    ///            〜の場合：入力を１つ捨て、nilを返す。(後続処理をスキップ)
    ///            〜場合：入力を真偽判定し、結果を返す。入力が拾えなければ、エラー
    ///            （入力から値(.value)が取れない場合、エラー(想定外)
    private func getCondition(from environment: Environment) -> JpfObject? {
        if let params = environment.peek(2) {
            if params[0].isParticle(.GA) && params[1].isParticle(.NO) {
                guard let left = params[0].value, let right = params[1].value else {return caseConditionError}
                environment.drop()
                var result: JpfObject
                switch right {
                case let array as JpfArray:
                    result = array.contains(left)
                case let range as JpfRange:
                    result = range.contains(left)
                default:
                    result = JpfBoolean.object(of: left.isEqual(to: right))
                }
                if alternative != nil || result.isTrue {environment.drop()} // 条件成立、または「それ以外」がある場合は、「〜が」を捨てる
                return result
            }
        }
        if environment.peek?.particle == .particle(.NO) {        // 「〜が」が捨てられていた場合
            environment.drop()
            return nil
        }
        guard let result = environment.pull() else {return caseConditionError}
        return JpfBoolean.object(of: result.isTrue)
    }
}
extension LogicalExpression : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        guard let condition = environment.peek else {return "「\(tokenLiteral)」" + logicalConditionError}
        guard let object = right.evaluated(with: environment) else {return "「\(tokenLiteral)」" + logicalRightEvaluationError}
        if object.isError {return object}
        guard let right = environment.pull() else {return "「\(tokenLiteral)」" + logicalRightEvaluationError}
        environment.drop()      // 入力（左辺(条件)）を捨てる
        switch token {
        case .keyword(.OR):
            return JpfBoolean.object(of: condition.isTrue || right.isTrue)
        case .keyword(.AND):
            return JpfBoolean.object(of: condition.isTrue && right.isTrue)
        default:
            return "「\(tokenLiteral)」" + logicalOperatorError
        }
    }
}
extension LoopExpression : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        // 入力（パラメータ）の数と、格・型をチェック
        if let params = environment.peek(3),
           params[0].isParticle(.KARA) && params[1].isParticle(.MADE) {
            environment.drop(3)
            return evaluatedLoop(from: params[0].number, through: params[1].number, by: params[2].number, with: environment)
        } else
        if let params = environment.peek(2) {
            guard params[0].isParticle(.KARA) && params[1].isParticle(.MADE) else {return strideLoopUsage}
            environment.drop(2)
            return evaluatedLoop(from: params[0].number, through: params[1].number, with: environment)
        } else
        if let param = environment.peek {
            switch param.value {
            case let array as JpfArray:
                return evaluatedLoop(of: array, with: environment)
            case let dictionary as JpfDictionary:
                return evaluatedLoop(of: dictionary, with: environment)
            case let range as JpfRange:
                return evaluatedLoop(of: range, with: environment)
            default:
                return evaluatedLoop(with: environment)
            }
        } else {
            return evaluatedLoop(with: environment)
        }
    }
    // 反復【(条件が<条件式>(の)間、)<処理>】。
    // (条件が無い場合、処理に「中止する」があることが前提→無いと無限ループ)
    private func evaluatedLoop(with environment: Environment) -> JpfObject? {
        var result = evaluatedCondition(with: environment)
        while result.isTrue && !result.isError {
            if let evaluated = body.evaluated(with: environment) {
                if isTerminated(by: evaluated) {break}  // 「中止する」による中止
                if evaluated.isBreakFactor {return evaluated}
            }   // エラーまたは中止でループ終了
            result = evaluatedCondition(with: environment)
        }
        guard !result.isError else {return result}
        return (result.isReturnValue && result.value != nil) ? result : nil
    }   // 「返す」で上がってきたReturnValueであれば、そのまま返す。それ以外は処理終了
    // <数値>から<数値>まで（<数値>ずつ）反復【入力が<識別子(カウント値)>、<処理>】。
    private func evaluatedLoop(from: Int?, through: Int?, by: Int? = 1, with environment: Environment) -> JpfObject? {
        guard let f = from, let t = through, let b = by,
              parameters.count == 1 else {return loopParameterError + strideLoopUsage}
        for counter in stride(from: f, through: t, by: b) {
            environment[parameters[0].value] = JpfInteger(value: counter)   // カウンタの更新
            if let evaluated = body.evaluated(with: environment) {
                if isTerminated(by: evaluated) {break}  // 「中止する」による中止
                if evaluated.isBreakFactor {return evaluated}
            }
        }
        return nil
    }
    // <範囲>を反復【入力が<識別子(カウント値)>、<処理>】。
    private func evaluatedLoop(of range: JpfRange, with environment: Environment) -> JpfObject? {
        guard parameters.count == 1 else {return loopParameterError + rangeLoopUsage}
        guard let lowerBound = range.lowerBound?.0.number,
              var upperBound = range.upperBound?.0.number,
              range.checked.isTrue else {return rangeLoopUsage}         // 上下限があり、形式が正しいこと
        environment.drop()
        if range.upperBound?.1 == .keyword(.UNDER) {upperBound -= 1}    // 未満なので、上限 - 1
        return evaluatedLoop(from: lowerBound, through: upperBound, with: environment)  // 下限から、上限までループ
    }
    // <配列>を反復【入力が<識別子(値)>、<処理>】。
    private func evaluatedLoop(of array: JpfArray, with environment: Environment) -> JpfObject? {
        guard parameters.count == 1 else {return loopParameterError + arrayLoopUsage}
        environment.drop()
        for element in array.elements {
            environment[parameters[0].value] = element
            if let evaluated = body.evaluated(with: environment) {
                if isTerminated(by: evaluated) {break}  // 「中止する」による中止
                if evaluated.isBreakFactor {return evaluated}
            }
        }
        return nil
    }
    // <辞書>を反復【入力が<識別子(キー)>と<識別子(値)>、<処理>】。
    private func evaluatedLoop(of dictionary: JpfDictionary, with environment: Environment) -> JpfObject? {
        guard parameters.count == 2 else {return loopParameterError + dictionaryLoopUsage}
        environment.drop()
        for pair in dictionary.pairs {
            environment[parameters[0].value] = pair.value.key
            environment[parameters[1].value] = pair.value.value
            if let evaluated = body.evaluated(with: environment) {
                if isTerminated(by: evaluated) {break}  // 「中止する」による中止
                if evaluated.isBreakFactor {return evaluated}
            }
        }
        return nil
    }
    // 「反復」の条件を評価する。条件式が無い場合は、真オブジェクトを返す。
    private func evaluatedCondition(with environment: Environment) -> JpfObject {
        if condition.isEmpty {return JpfBoolean.TRUE}
        var result: JpfObject?
        for expression in condition {
            result = expression.evaluated(with: environment)
            if let object = result, object.isBreakFactor {break}
            result.map {environment.push($0)}
        }
        environment.drop()                              // 真偽値を捨てる
        return result ?? conditionEvaluationError
    }
    // ループが「中止する」で中止されたか判定
    private func isTerminated(by evaluated: JpfObject) -> Bool {evaluated.isReturnValue && !evaluated.hasValue}
}
extension Label : Evaluatable {
    /// 1. ラベルに割り当てられた識別子を辞書に格納する。
    /// 2. ラベルが「位置」である場合は、数値、または識別子から数値を取り出し、返す。
    /// - Parameter environment: 格納先
    /// - Returns: ラベルの文字列、または位置の数値
    func evaluated(with environment: Environment) -> JpfObject? {
        if token == .keyword(.POSITION) {
            if let integer = Int(value) {
                return JpfInteger(name: tokenLiteral, value: integer)
            }
            guard var integer = environment[value] as? JpfInteger else {return "『\(value)』" + identifierNotFound}
            integer.name = tokenLiteral
            return integer
        }
        environment[tokenLiteral] = JpfString(value: value)
        return JpfString(value: tokenLiteral)
    }
}
extension ArrayLiteral : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        var results: [JpfObject] = []
        for element in elements {
            if let result = element.evaluated(with: environment), result.isError {return result}
            if let params = environment.peek(2), let elements = elements(from: params) {
                environment.drop(2)
                results = elements
                break
            }
            results.append(environment.pull()!)     // スタックに積まれた評価結果を回収
        }
        return JpfArray(elements: results)
    }
    private func elements(from objects: [JpfObject]) -> [JpfObject]? {
        // 同一要素を指定個数で(例：配列【10個の０】）
        guard let phrase = objects.first as? JpfPhrase, phrase.isNumber && phrase.isParticle(.KO) else {return nil}
        return Array(repeating: objects[1], count: phrase.number!)
    }
}
extension DictionaryLiteral : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        var results: [JpfHashKey: (key: JpfObject, value: JpfObject)] = [:]
        for expression in pairs {
            guard let key = expression.pair.key.evaluated(with: environment) else {return nil}
            guard !key.isError else {return key}
            let keyObject = environment.pull()!     // スタックに積まれた評価結果を回収
            guard let object = key as? JpfHashable else {return unusableAsHashKey + "「\(key.string)」"}
            guard let value = expression.pair.value.evaluated(with: environment) else {return nil}
            guard !value.isError else {return value}
            let valueObject = environment.pull()!   // スタックに積まれた評価結果を回収
            results[object.hashKey] = (keyObject, valueObject)
        }
        return JpfDictionary(pairs: results)
    }
}
extension FunctionLiteral : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        accessed(with: environment) ?? JpfFunction(parameters: parameters, signature: signature, body: body, environment: environment)
    }
}
extension JpfFunction {
    /// 関数を実行する。
    /// スタックの引数は、関数ローカルの辞書に登録される。(local.apply())
    /// 本体は評価実行され、エラーかアンラップされた返り値を返す。
    /// - Parameter environment: 実行中の(通常もしくは関数の)環境
    /// - Returns: エラーかアンラップされた返り値、なければnil
    func executed(with environment: Environment) -> JpfObject? {
        let local = Environment(outer: self.environment)    // 関数の環境を拡張
        let stackEnv = environment.outer != nil ? environment : self.environment
        let result = local.apply(parameters, with: signature, from: stackEnv)
        guard !result.isError else {return result}
        defer {environment.push(local.pullAll())}           // スタックを戻す
        if let evaluated = Evaluator(from: body, with: local).object {
            guard !evaluated.isError else {return evaluated}
            return unwrappedReturnValue(of: evaluated)
        }
        return nil
    }
    /// 関数で返す値がラップされていたら、アンラップして返す。
    /// (アンラップしないと、呼び出し元の処理が止まってしまう)
    /// - Parameter object: 返す値
    /// - Returns: アンラップされた値、またはnil (すでにスタックに積まれているはので値は返さない)
    private func unwrappedReturnValue(of object: JpfObject) -> JpfObject? {
        object.isReturnValue ? object.value! : nil
    }
}
extension JpfArray {
    func executed(with environment: Environment) -> JpfObject? {
        for element in elements.reversed() {    // 逆順に入力形式をチェック
            guard let function = element as? JpfFunction else {return notExecutableObject + element.type}
            if let number = function.signature.numberOfInputs { // 固定長入力
                guard let params = environment.peek(number) else {continue}
                for (param, designated) in zip(params, function.signature.formats) {
                    if environment.isSameType(of: param, as: designated.type) &&
                        environment.isSameParticle(of: param, as: designated.particle) {
                        return function.executed(with: environment)
                    }
                }
            } else {                                            // 可変長入力
                for designated in function.signature.formats {
                    if contains(designated, in: environment) {
                        return function.executed(with: environment)
                    }
                }
            }
        }
        return functionParameterError
    }
    private func contains(_ designated: (String, String), in environment: Environment) -> Bool {
        let particle = designated.1.hasSuffix("…") ? String(designated.1.dropLast(1)) : designated.1
        return environment.getAll().contains { object in
            environment.isSameType(of: object, as: designated.0) && environment.isSameParticle(of: object, as: particle)
        }
    }
    private var notExecutableObject: JpfError   {JpfError("「関数」以外を実行しようとした。型：")}
    private var functionParameterError: JpfError{JpfError("「関数」の入力が指定形式と一致しない。")}
}
extension TypeLiteral : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        if let error = accessed(with: environment) {return error}
        switch derivedProtocols(from: protocols, with: environment) {
        case .success(let all):
            let local = Environment(outer: environment)
            if let members = typeMembers,
               let result = Evaluator(from: members, with: local).object, result.isError {return result}
            return JpfType(parameters: parameters, signature: signature, initializer: initializer, environment: local, protocols: all, body: body)
        case .failure(let error):
            return error.message
        }
    }
}
extension ProtocolLiteral : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        if let error = accessed(with: environment) {return error}
        switch derivedProtocols(from: protocols, with: environment) {
        case .success(let all):
            return JpfProtocol(protocols: all, clauses: clauses)
        case .failure(let error):
            return error.message
        }
    }
}
func derivedProtocols(from protocols: [String], with environment: Environment) -> Result<[String], ConformityError> {
    var results: [String] = protocols
    for s in protocols {
        guard let p = environment[s] as? JpfProtocol else {return .failure(.notFound(protocol: s))}
        for d in p.protocols {
            guard !protocols.contains(d) else {return .failure(.duplicated(protocol: d))}
            results.append(d)
        }
    }
    return .success(results)
}
enum ConformityError : Error {
    case notFound(protocol: String)
    case duplicated(protocol: String)
    var message: JpfError {
        switch self {
        case .notFound(let s):      return JpfError("準拠する規約の識別子が見つからない。：" + s)
        case .duplicated(let s):    return JpfError("準拠する規約の識別子が重複している。：" + s)
        }
    }
}
extension EnumLiteral : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        let local = Environment(outer: environment)             // 列挙型の環境を拡張
        var ident: String = ""
        var identifiers: [String] = []
        var number = 0                  // デフォルトの連番
        for element in elements {
            if let statement = element as? ExpressionStatement,
               let expression = statement.expressions.first {   // 値無し(数値を値として設定)
                guard expression is Identifier else {return enumuratorError + element.string}
                ident = expression.tokenLiteral
                local[ident] = JpfInteger(name: ident, value: number)   // 値として連番を登録
                number += 1
            } else
            if let statement = element as? DefineStatement {    // 値有り(値をlocalに設定)
                ident = statement.name.value
                if let result = element.evaluated(with: local), result.isError {return result}
                if let value = local[ident] as? JpfInteger {    // 設定値が<数値>の場合、
                    number = value.value + 1                    // 値+1を連番に振り直す。
                }
            } else {
                return enumuratorError + element.string
            }
            identifiers.append(ident)
        }
        return JpfEnum(elements: identifiers, environment: local)
    }
}
