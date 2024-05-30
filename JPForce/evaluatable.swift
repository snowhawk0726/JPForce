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
    var conditionalOperationError: JpfError {JpfError("「によって」の条件(真偽値)が見つからなかった。")}
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
    var functionOverloadError: JpfError     {JpfError("「関数」を「関数」以外で多重定義している。")}
    var computationOverloadError: JpfError     {JpfError("「算出」を「算出」以外で多重定義している。")}
    var typeExtentionError: JpfError        {JpfError("「型」を「型」以外で拡張している。")}
    var protocolExtentionError: JpfError    {JpfError("「規約」を「型」以外で拡張している。")}
    var cannotExtend: JpfError              {JpfError("拡張することはできない。")}
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
        guard var definition = environment.pull() else {return nil}
        if isExtended && environment.contains(name.value) {
            definition = extend(environment, by: name.value, with: definition)
            if definition.isError {return definition}
        }
        environment[name.value] = definition
        return nil
    }
    /// 元の定義(environment[name])を、新しい定義(object)で拡張(または多重定義する)
    /// - Parameters:
    ///   - environment: 元の環境
    ///   - name: 拡張される識別子
    ///   - object: 拡張するオブジェクト(定義)
    /// - Returns: 拡張されたオブジェクト(定義)、またはエラー
    ///   (元の環境に識別子が無い、もしくは非対称である場合は、objectを返す。)
    private func extend(_ environment: Environment, by name: String, with object: JpfObject) -> JpfObject {
        switch environment[name] {              // 元の定義
        case let original as JpfFunction:       // 関数の多重定義
            guard let function = object as? JpfFunction else {return functionOverloadError}
            return JpfFunction(
                name: original.name,
                functions: overload(original.functions, with: function.functions.overloaded),
                environment: environment
            )
        case let original as JpfComputation:    // 算出の多重定義
            guard var computation = object as? JpfComputation else {return computationOverloadError}
            if computation.getters.array.count == 1 && computation.setters.isEmpty {
                // 算出定義内に取得が唯一 → <識別子>は、さらに、算出【取得は、さらに、【…】】
                computation.getters = computation.getters.overloaded
            }
            return JpfComputation(
                name: original.name,
                setters: overload(original.setters, with: computation.setters),
                getters: overload(original.getters, with: computation.getters),
                environment: environment
            )
        case let original as JpfType:           // 型の拡張
            guard let type = object as? JpfType else {return typeExtentionError + "(拡張先が\(object.type))"}
            let result = extend(original, by: type)
            if result.isError {return result}
            return result
        case let orignal as JpfProtocol:        // 規約デフォルト実装
            guard let extended = object as? JpfType else {return protocolExtentionError + "(拡張先が\(object.type))"}
            return JpfProtocol(
                protocols: orignal.protocols,
                clauses: orignal.clauses,
                body: extended.body
            )
        default:
            break
        }
        return object
    }
    /// 元の型を、新しい型の定義で拡張する。
    /// - Parameters:
    ///   - original: 元の型
    ///   - definition: 拡張する型の定義
    /// - Returns: 拡張された型、またはエラー
    private func extend(_ original: JpfType, by definition: JpfType) -> JpfObject {
        var extended = original
        // 初期化の追加(多重定義)
        extended.initializers = overload(original.initializers, with: definition.initializers)
        // 型の環境を統合(メソッドの多重定義を考慮)
        for (k, v) in definition.environment.enumerated {
            let object = extend(original.environment, by: k, with: v)
            if object.isError {return object}
            extended.environment[k] = object
        }
        // 規約の統合
        for p in definition.protocols {
            guard !original.protocols.contains(p) else {return ConformityError.duplicated(protocol: p).message}
            extended.protocols.append(p)
        }
        // インスタンスの定義を統合
        if let statements = definition.body?.statements {
            extended.body?.statements += statements
        }
        return extended
    }
    private func overload(_ original: FunctionBlocks, with definition: FunctionBlocks) -> FunctionBlocks {
        var overloaded = original
        return overloaded.append(definition)
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
    /// - オブジェクトが設定の取得の場合、実行結果(返り値またはエラー)を返す。
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
        var object: JpfObject
        if let result = left.evaluated(with: environment) { // leftの評価結果が値を返した場合
            object = result
        } else
        if let value = environment.unwrappedPeek {          // leftがスタックに値を出力した場合
            environment.drop()
            object = value
        } else {
            return nil
        }
        guard !object.isError else {return object}
        return JpfPhrase(value: object, particle: token)
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
extension ConditionalOperation : Evaluatable {
    func evaluated(with environment: Environment) -> (any JpfObject)? {
        guard let condition = environment.unwrappedPeek else {return conditionalOperationError}
        environment.drop()
        return condition.isTrue ?
            consequence.evaluated(with: environment) :
            alternative.evaluated(with: environment)
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
        while let result = evaluatedCondition(with: environment),
                result.isTrue && !result.isError {
            if let evaluated = body.evaluated(with: environment) {
                if evaluated.isBreak {break}        // 中止する
                if evaluated.isContinue {continue}  // 継続する
                if evaluated.isBreakFactor {return evaluated}
            }
        }
        guard let result = evaluatedCondition(with: environment) else {return conditionEvaluationError}
        guard !result.isError else {return result}
        return nil
    }   // 「返す」で上がってきたReturnValueであれば、そのまま返す。それ以外は処理終了
    // <数値>から<数値>まで（<数値>ずつ）反復【入力が<識別子(カウント値)>、<処理>】。
    private func evaluatedLoop(from: Int?, through: Int?, by: Int? = 1, with environment: Environment) -> JpfObject? {
        guard let f = from, let t = through, let b = by,
              parameters.count == 1 else {return loopParameterError + strideLoopUsage}
        for counter in stride(from: f, through: t, by: b) {
            environment[parameters[0].value] = JpfInteger(value: counter)   // カウンタの更新
            if let evaluated = body.evaluated(with: environment) {
                if evaluated.isBreak {break}        // 中止する
                if evaluated.isContinue {continue}  // 継続する
                if evaluated.isBreakFactor {return evaluated}
            }
        }
        return nil
    }
    // <範囲>を反復【入力が<識別子(カウント値)>、<処理>】。
    private func evaluatedLoop(of range: JpfRange, with environment: Environment) -> JpfObject? {
        guard parameters.count == 1 else {return loopParameterError + rangeLoopUsage}
        guard let lowerBound = range.lowerBound?.0.number,
              var upperBound = range.upperBound?.0.number else {return rangeLoopUsage}
        if let formatError = range.error {return formatError}
        environment.drop()
        if range.upperBound?.1 == Token(.UNDER) {upperBound -= 1}       // 未満なので、上限 - 1
        return evaluatedLoop(from: lowerBound, through: upperBound, with: environment)  // 下限から、上限までループ
    }
    // <配列>を反復【入力が<識別子(値)>、<処理>】。
    private func evaluatedLoop(of array: JpfArray, with environment: Environment) -> JpfObject? {
        guard parameters.count == 1 else {return loopParameterError + arrayLoopUsage}
        environment.drop()
        for element in array.elements {
            environment[parameters[0].value] = element
            if let evaluated = body.evaluated(with: environment) {
                if evaluated.isBreak {break}        // 中止する
                if evaluated.isContinue {continue}  // 継続する
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
                if evaluated.isBreak {break}        // 中止する
                if evaluated.isContinue {continue}  // 継続する
                if evaluated.isBreakFactor {return evaluated}
            }
        }
        return nil
    }
    // 「反復」の条件を評価する。条件式が無い場合は、真オブジェクトを返す。
    private func evaluatedCondition(with environment: Environment) -> JpfObject? {
        if condition.isEmpty {return JpfBoolean.TRUE}
        var result: JpfObject?
        for expression in condition {
            result = expression.evaluated(with: environment)
            if let object = result, object.isBreakFactor {break}
            result.map {environment.push($0)}
        }
        environment.drop()                          // 真偽値を捨てる
        return result
    }
}
extension Label : Evaluatable {
    /// 1. ラベルに割り当てられた識別子(名)を辞書に格納する。
    /// 2. ラベルが「位置」である場合は、数値、または識別子から取り出した数値、を返す。
    /// - Parameter environment: 格納先
    /// - Returns: 識別子名、または位置の数値 (オブジェクトの識別子名は、ラベル名)
    func evaluated(with environment: Environment) -> JpfObject? {
        var object: JpfObject
        if token == .keyword(.POSITION) {
            if let integer = Int(value.literal) {
                object = JpfInteger(name: tokenLiteral, value: integer)
            } else {
                guard let integer = environment[value.literal] as? JpfInteger else {return "『\(value.literal)』" + identifierNotFound}
                object = JpfInteger(name: tokenLiteral, value: integer.value)
            }
        } else {
            object = JpfString(name: tokenLiteral, value: value.literal)
            environment.append(object, to: tokenLiteral)
        }
        return object
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
            if environment.isEmpty {return elementEvalError + "空"}
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
        var results: [JpfHashKey : (key: JpfObject, value: JpfObject)] = [:]
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
        accessed(with: environment) ?? JpfFunction(functions: functions, environment: environment)
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
        let stackEnv = environment.isEmpty ? self.environment : environment
        defer {environment.push(stackEnv.pullAll())}        // スタックを戻す
        if let returnValue = stackEnv.execute(functions, with: local) {
            return returnValue
        }
        return nil
    }
}
extension ComputationLiteral : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        accessed(with: environment) ?? JpfComputation(setters: setters, getters: getters, environment: environment)
    }
}
extension JpfComputation {
    /// 算出(取得)を行う。
    /// - Parameter environment: 実行中の(通常もしくは算出の)環境
    /// - Returns: エラーかアンラップされた返り値、なければnil
    func getter(with environment: Environment) -> JpfObject? {
        guard !getters.isEmpty else {return getterNotFound}
        let local = Environment(outer: self.environment)    // 環境を拡張
        return environment.execute(getters, with: local)
    }
    /// 算出(設定)を行う。
    /// - Parameter environment: 実行中の(通常もしくは算出の)環境
    /// - Returns: エラーかアンラップされた返り値、なければnil
    func setter(with environment: Environment) -> JpfObject? {
        guard !setters.isEmpty else {return setterNotFound}
        let local = Environment(outer: self.environment)    // 環境を拡張
        return environment.execute(setters, with: local)
    }
}
extension TypeLiteral : Evaluatable {
    func evaluated(with environment: Environment) -> JpfObject? {
        if let error = accessed(with: environment) {return error}
        switch derivedProtocols(from: protocols, with: environment) {
        case .success(let all):
            let local = Environment(outer: environment)
            if let members = typeMembers,
               let result = Evaluator(from: members, with: local).object, result.isError {return result}
            if let result = local.conform(to: all, isTypeMember: true), result.isError {return result}
            return JpfType(initializers: initializers, environment: local, protocols: all, body: body)
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
private func derivedProtocols(from protocols: [String], with environment: Environment) -> Result<[String], ConformityError> {
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
