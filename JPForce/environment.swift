//
//  environment.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/03/06.
//
import Foundation

class Environment {
    init(outer: Environment? = nil) {self.outer = outer; self.stack = Stack()}
    init(with stack: Stack) {self.outer = nil; self.stack = stack}
    let outer: Environment?     // 拡張環境
    static let OUTER = Token.Keyword.OUTER.rawValue // 外部
    var isExecutable: Bool = true                   // false: 実行抑止
    private var store: [String: JpfObject] = [:]
    private var stack: Stack
    var redefineds: Set<Token.Keyword> = []         // 再定義された予約語
    private var arguments: [String: JpfObject] = [:]// 引数
    // MARK: - 辞書操作
    subscript(_ name: String) -> JpfObject? {
        get {store[name] ?? outer?[name]}           // 外部環境の取得は可
        set {
            var object = newValue
            object?.name = name                     // 識別子名をオブジェクトに記録
            store[name] = object
        }
    }
    var enumerated: [(key: String, value: JpfObject)] {
        store.map {(key: $0, value: $1)}
    }
    var values: [JpfObject] {Array(store.values) + (outer?.values ?? [])}
    /// storeを、JpfDictionaryに変換する。(値は、store[key].stirng)
    var stringDictionary: JpfDictionary {
        let keys = store.keys.map {JpfString(value: $0).hashKey}
        let values = store.map {(key: JpfString(value: $0), value: JpfString(value: $1.string))}
        return JpfDictionary(pairs: Dictionary(uniqueKeysWithValues: zip(keys, values)))
    }
    func contains(_ name: String) -> Bool {store.keys.contains(name)}
    /// 辞書にオブジェクトを追加する。
    ///     未登録のオブジェクトであれば、そのまま辞書に登録
    ///     登録済みのオブジェクトが配列(JpfArray)であれば、配列の要素に追加
    ///     それ以外は、登録済みオブジェクトと追加オブジェクトの配列(JpfArray)を作り、配列を辞書に登録
    /// - Parameters:
    ///   - object: 追加するオブジェクト
    ///   - name: 追加対象の識別子名
    func append(_ object: JpfObject, to name: String) {
        if contains(name) {
            if var array = store[name] as? JpfArray, array.name == name {
                array.elements.append(object)
            } else {
                self[name] = JpfArray(elements: [store[name]!, object])
            }
            return
        }
        store[name] = object
    }
    /// 辞書からオブジェクトを取り出す。
    /// - Parameter name: 対象の識別子名
    /// - Returns: 取り出したオブジェクト(もしくはnil)
    func retrieve(name: String) -> JpfObject? {
        var object = store[name]
        if let array = object as? JpfArray, array.name == name {
            object = array.elements.first
        }
        return object
    }
    /// 辞書から識別子に対応したオブジェクトを削除する。
    /// - Parameter name: 対象の識別子名
    func remove(name: String) {
        if var array = store[name] as? JpfArray, array.name == name {
            array.elements.removeFirst()
            if array.elements.count == 1 {
                store[name] = array.elements.first
            }
        } else {
            self[name] = nil
        }
    }
    func assign(_ value: JpfObject, with name: String) -> JpfObject? {
        guard !name.isEmpty else {return value}
        if contains(Self.OUTER) {           // 外部識別子
            guard let outer = outer, outer.contains(name) else {
                return JpfError("「外部」の辞書が無い、または、『\(name)』(識別子)が定義されていない。")
            }
            outer[name] = value             // 外部に代入
            remove(name: Self.OUTER)        // 外部ラベルを削除
        } else
        if contains(name) {                 // ローカルにある
            self[name] = value              // 代入
        } else
        if let outer = outer,
           outer.contains(name) {           // 外部にある
            outer[name] = value             // 外部に代入
        } else {
            return JpfError("『\(name)』(識別子)が定義されていない。")
        }
        return nil
    }
    func assign(_ value: JpfObject, with object: JpfObject?) -> JpfObject? {
        guard let object = object else {return JpfError("代入先の値が存在しない。")}
        let name = object.name
        if !name.isEmpty {                  // 既存識別子への代入
            return assign(value, with: name)
        } else {                            // 定義
            if contains(Self.OUTER) {
                return JpfError("外部『\(name)』(識別子)が定義されていない。")
            }
            self[getName(from: object)] = value // 識別子に値を登録
        }
        return nil
    }
    func storeArguments(with args: Environment, shouldMerge: Bool = false) {
        args.enumerated.forEach {
            arguments[$0] = $1
            if shouldMerge {self[$0] = $1}
        }
    }
    var argumentPairs: [(String, JpfObject)] {
        arguments.map {($0, $1)}
    }
    func contains(_ keyword: Token.Keyword) -> Bool {
        if redefineds.contains(keyword) {return true}
        guard let instance = peek?.value as? JpfInstance else {return false}
        return instance.available.contains(keyword.rawValue)
    }
    // MARK: - スタック操作
    func push(_ object: JpfObject) -> JpfError?     {stack.push(object)}
    func push(_ objects: [JpfObject]) -> JpfError?  {stack.push(objects)}
    func pull() -> JpfObject?                       {stack.pull()}
    /// スタックに指定のオブジェクトがあったら、それを取り出す。(無ければ nil)
    func pull(where condition: (JpfObject) -> Bool) -> JpfObject? {stack.pull(where: condition)}
    func pullAll() -> [JpfObject]                   {stack.pullAll()}
    func getAll() -> [JpfObject]                    {stack.getAll()}
    func drop()                                     {stack.drop()}
    func drop(_ n: Int)                             {stack.drop(n)}
    func empty()                                    {stack.empty()}
    func swap()                                     {stack.swap()}
    //
    var isEmpty: Bool                               {stack.isEmpty}
    var count: Int                                  {stack.count}
    var peek: JpfObject?                            {stack.peek()}
    func peek(_ n: Int) -> [JpfObject]?             {stack.peek(n)}
    subscript(index: Int) -> JpfObject?             {stack[index]}
    var string: String                              {stack.string}
    // MARK: - 入力操作
    var isPeekParticle: Bool {peek?.particle != nil}
    func isPeekParticle(_ particle: Token.Particle) -> Bool {
        if case .particle(particle) = peek?.particle {return true}
        return false
    }
    var unwrappedPeek: JpfObject? {
        if peek is JpfPhrase {return peek?.value}
        return peek
    }
    var unwrappedValue: JpfObject? {
        guard let phrase = peek as? JpfPhrase else {return nil}
        drop()
        return phrase.value
    }
    func getName(from object: JpfObject?) -> String {
        var name = object?.value?.name ?? ""
        if name.isEmpty, let string = object?.value as? JpfString {name = string.value}
        return name
    }
    func getName() -> String {return getName(from: peek)}
    /// 多重定義の関数ブロックから、引数形式が一致するものを抽出し、処理を行う。
    /// - Parameters:
    ///   - functions: 処理ブロック(【入力が〜、本体が〜。】)
    ///   - local: 関数処理を行う環境 (selfは入力環境)
    /// - Returns: 処理結果、エラー、またはnil
    func execute(_ functions: FunctionBlocks, with local: Environment) -> JpfObject? {
        for n in stride(from: count, through: -1, by: -1) {     // スタック上の引数の数毎
            for function in functions[n].reversed() {           // 引数の数に応じた多重定義毎
                let designated = function.paramForm
                if hasParameters(to: designated, number: n) {   // 引数が一致する入力を処理
                    if let error = local.apply(function, numberOfArguments: n, from: self) {
                        return error
                    }
                    return execute(function, with: local)
                }
            }
        }
        if let keyword = functions.keyword {        // 予約語再定義の既定動作
            let predicate = PredicateOperableFactory.create(from: keyword, with: self)
            return predicate.operated()
        }
        if functions.array.count == 1 {             // 単体の定義の場合、エラーメッセージを作成する
            return errorMessage(to: functions.array.last!)
        }
        return InputFormatError.noMatchingSignature.message
    }
    /// 多重定義の関数ブロックから、引数形式が一致するものを抽出し、実行する。
    /// - Parameters:
    ///   - functions: 対象の多重定義
    ///   - local: 関数の実行環境
    /// - Returns: 処理結果、エラー、またはnil
    func call(_ functions: FunctionBlocks, with local: Environment) -> JpfObject? {
        if let function = functions.function(with: local) {
            if let error = local.apply(function) {  // 残りの引数を割り当てる
                return error
            }
            return execute(function, with: local)
        }
        if let keyword = functions.keyword {        // 予約語再定義の既定動作
            let predicate = PredicateOperableFactory.create(from: keyword, with: self)
            return predicate.operated()
        }
        if functions.array.count == 1 {             // 単体の定義の場合、エラーメッセージを作成する
            let f = functions.array.last!           // 最新定義
            let n = local.store.count               // 指定引数の数
            guard functions.dictionary.keys.contains(n) || f.paramForm.numberOfInputs == nil else {
                let n = f.parameters.count - f.paramForm.numberOfDefaultValues
                return InputFormatError.numberOfParameters(n).message
            }
            return errorMessage(to: f, with: local)
        }
        return InputFormatError.noMatchingSignature.message
    }
    /// 関数のローカル環境で、関数を実行する。
    /// - Parameters:
    ///   - function: 実行する関数ブロック
    ///   - local: 関数のローカル環境
    /// - Returns: 返り値(無いときはnil)、またはエラー
    private func execute(_ function: FunctionBlock, with local: Environment) -> JpfObject? {
        defer {_ = push(local.pullAll())}   // スタックを戻す
        if let body = function.body, let result = Evaluator(from: body, with: local).object {
            if let error = local.errorMessage(to: function.returnTypes, with: result) {    // 返り値の型をチェック
                return error
            }
            if result.isReturnValue {return result.value}
            if result.isError {return result}
        } else {
            return local.errorMessage(to: function.returnTypes)
        }
        return nil
    }
    /// スタック上の引数(オブジェクト)と指定された形式が一致するかをチェックする。
    /// - Parameters:
    ///   - signature: 指定された形式(引数の数および型と格)
    ///   - number:    引数の数(-1: 不明)
    /// - Returns: true: 一致、または引数無し、
    ///            false: 不一致、または引数が指定未満
    func hasParameters(to signature: InputFormat, number: Int) -> Bool {
        guard let arguments = number >= 0 ? peek(number) : getAll() else {
            return false                            // 引数が指定に満たない
        }
        switch number {
        case -1:                                    // 可変長引数あり
            for (format, value) in zip(signature.formats, signature.values) {
                if format.hasThreeDots {            // 可変長の入力
                    continue                        // 引数のシグネチャにかかわらず、チェックok
                } else {                            // 固定長の入力
                    if value != nil {continue}      // 既定値あり
                    if !arguments.contains(where: {isSameFormat(of: $0, as: format)}) {
                        return false                // 型または格が一致する引数が無い
                    }
                }
            }
        case 0:                                     // 引数無し(チェック不要)
            break
        default:                                    // 固定長引数
            for (argument, format) in zip(arguments, signature.formats) {
                if !isSameFormat(of: argument, as: format) {
                    return false                    // 引数と型または格が一致しない
                }
            }
        }
        return true
    }
    /// 外部のスタック上の引数(オブジェクト)の形式チェックを行い値を取り出し、各識別子に引き当て、内部(self)の辞書に登録する。
    /// - Parameters
    ///   - function: 対象の関数ブロック
    ///   - numberOfArguments: スタックから引き当てる引数の数
    ///   - outer:  引数を格納しているスタックを含む環境
    /// - Returns: 引数がパラメータおよび指定形式に合わない場合は、InputFormatError(JpfError)を返す。
    private func apply(_ function: FunctionBlock, numberOfArguments: Int, from outer: Environment) -> JpfError? {
        let designated = function.paramForm
        guard let arguments = numberOfArguments >= 0 ? outer.peek(numberOfArguments) : outer.getAll() else {
            return InputFormatError.noParameterValue.message
        }                                           // 引数を取得
        var a = 0                                   // 引数の位置
        if numberOfArguments < 0 {                  // 可変長の識別子に対応する位置を求める
            guard let format = designated.firstParamFormat else {   // 最初の(既定値の無い)入力形式
                return InputFormatError.notFoundFirstFormat.message
            }
            a = arguments.firstIndex {isSameFormat(of: $0, as: format)} ?? 0
        }
        for (i, idetifier) in function.parameters.enumerated() {    // 入力の位置
            let format = designated.formats[i]      // 指定形式
            var value: JpfObject                    // 割り当てる値
            if !format.hasThreeDots {               // 固定長
                if a < arguments.count, let argument = arguments[a].value {
                    value = argument                // 値を引数から取得
                    a += 1
                } else {                            // 値を既定値から取得
                    value = getDefaultValue(with: designated, at: i)
                }
            } else {                                // 可変長
                let remainingArgs = Array(arguments[a..<arguments.count])
                let length = numberOfElements(in: remainingArgs, withConsecutive: format)
                let elements = remainingArgs.prefix(length).map {$0.value ?? JpfNull.object}    // 要素の値を取り出す。
                value = JpfArray(elements: elements)// 値を配列とする
                a += length
            }
            if value.isError {return value.error}
            self[idetifier.value] = value           // 値の割り当て
        }
        outer.drop(a)
        return nil
    }
    /// 辞書に登録済み引数以外を、既定値で登録する。
    /// - Parameter function: 対象の関数ブロック
    /// - Returns: エラー、無しはnil
    private func apply(_ function: FunctionBlock) -> JpfError? {
        let difference = Set(function.parameters.map {$0.value}).subtracting(arguments.keys)
        for name in difference {
            let i = function.index(of: name)!   // parametersに属するので、nilにはならない。
            self[name] = getDefaultValue(with: function.paramForm, at: i)
        }
        return nil
    }
    /// 指定された既定値式を評価し、値を得る
    /// - Parameters:
    ///   - designated: 指定の入力形式
    ///   - p: 既定値の位置
    /// - Returns: 既定値、またはエラー
    private func getDefaultValue(with designated: InputFormat, at p: Int) -> JpfObject {
        if let expression = designated.values[p] {
            guard let result = expression.evaluated(with: self) else {
                return InputFormatError.failedToEvaluate.message
            }
            if result.isError {return result}
            let type = designated.formats[p].type               // 指定の型
            if !type.isEmpty && type != result.type {
                return InputFormatError.typeOfDefaultValue.message
            }
            return pull()!
        }
        return InputFormatError.cannotGetDefault.message
    }
    /// 同じ指定格を持つ連続するオブジェクトの数を返す。
    /// - Parameters:
    ///   - elelments: 母集団(対象のオブジェクト配列)
    ///   - p: 指定格
    /// - Returns: オブジェクトの数
    private func numberOfElements(in elelments: [JpfObject], withConsecutive format: InputFormat.Format) -> Int {
        var counter = 0
        while (counter < elelments.count && isSameFormat(of: elelments[counter], as: format)) {counter += 1}
        return counter
    }
    private func isSameFormat(of object: JpfObject, as format: InputFormat.Format) -> Bool {
        return isSameType(of: object, as: format.type) && isSameParticle(of: object, as: format.particle)
    }
    /// 対象のオブジェクトの型をチェック
    private func isSameType(of object: JpfObject, as type: String) -> Bool {
        return type.isEmpty || object.value?.contains(type: type) ?? false
    }
    /// 対象のオブジェクトの格をチェック
    private func isSameParticle(of object: JpfObject, as particle: String) -> Bool {
        return particle.isEmpty || object.particle == Token(word: particle)
    }
    private func isSameParticle(of object: JpfObject, as particle: Token.Particle?) -> Bool {
        if let p = particle {
            return object.particle == Token(p)
        }
        return object.particle == nil
    }
    /// 対象のオブジェクトの識別子をチェック
    private func isSameName(of object: JpfObject, as name: String) -> Bool {
        return object.value?.name.isEmpty ?? true || object.value?.name == name
    }
    /// 形式から格を取得
    private func getParticle(from s: String) -> Token.Particle? {
        return Token.Particle(rawValue: s)
    }
    // MARK: - 準拠違反・エラー検出
    /// 関数ブロックのエラーをチェックし、結果を返す。
    /// - Parameter function: 対象の関数ブロック
    /// - Returns: エラー(無しの場合はnil)
    private func errorMessage(to function: FunctionBlock) -> JpfError? {
        let numberOfParameters = function.parameters.count
        if let numberOfInputs = function.paramForm.numberOfInputs { // 固定長入力指定
            guard numberOfInputs == numberOfParameters,
                  let parameters = self.peek(numberOfParameters) else {
                return InputFormatError.numberOfParameters(numberOfParameters).message
            }
            for (parameter, format) in zip(parameters, function.paramForm.formats) {
                if let error = errorMessage(to: parameter, with: format) {return error}
            }
        } else {                                                    // 可変長入力指定
            if self.isEmpty {return InputFormatError.numberOfParameters(numberOfParameters).message}
            let params = self.getAll()
            var n = params.count - 1
            for (format, value) in zip(function.paramForm.formats.reversed(),function.paramForm.values.reversed()) {
                guard n >= 0 else {break}                           // 引数が無い
                if format.hasThreeDots {                            // 可変長指定
                    return errorMessage(to: params[0..<n].reversed(), with: format)
                } else {
                    if value != nil {continue}
                    if let error = errorMessage(to: params[n], with: format) {return error}
                    n -= 1
                }
            }
        }
        return nil
    }
    /// 関数ブロックの引数と与えられた辞書を比較してエラーを返す。
    /// - Parameters:
    ///   - f: 関数ブロック
    ///   - args: 比較する辞書を含む環境
    /// - Returns: エラー(無ければnil)
    private func errorMessage(to f: FunctionBlock, with args: Environment) -> JpfError? {
        let pairs = args.enumerated
        for (k, v) in pairs {
            if f.hasVariableParameter(named: k) {   // 可変長引数
                guard v.type == JpfArray.type else {
                    return InputFormatError.variable(v.type).message
                }
            } else {                                // 固定長引数
                guard f.hasParameter(named: k,  ofType: v.type) else {
                    return InputFormatError.type(v.type).message
                }
            }
        }
        return nil
    }
    /// 可変長の引数のエラーチェック
    /// - Parameters:
    ///   - parameters: 引数
    ///   - format: チェックする入力形式
    /// - Returns: エラー、無ければnil
    private func errorMessage(to parameters: [JpfObject], with format: InputFormat.Format) -> JpfError? {
        guard let particle = getParticle(from: format.particle) else {  // 格をチェック
            return InputFormatError.particleFormat(format.particle).message
        }
        if let index = parameters.firstIndex(where: {!isSameParticle(of: $0, as: particle)}) {
            return InputFormatError.particle(parameters[index].particle?.literal ?? "無").message
        }
        if let index = parameters.firstIndex(where: {!isSameType(of: $0, as: format.type)}) {
            return InputFormatError.type(parameters[index].value?.type ?? "無").message
        }
        return nil
    }
    /// 引数のエラーチェック(型と格)
    /// - Parameters:
    ///   - parameter: 引数
    ///   - format: チェックする入力形式
    /// - Returns: エラー、無ければnil
    private func errorMessage(to parameter: JpfObject, with format: InputFormat.Format) -> JpfError? {
        if !isSameType(of: parameter, as: format.type) {return InputFormatError.type(parameter.value?.type ?? "無").message}
        if !isSameParticle(of: parameter, as: format.particle) {return InputFormatError.particle(parameter.particle?.literal ?? "無").message}
        return nil
    }
    /// 規約(protocols)に違反していたら、エラーを返す。
    func conform(to protocols: [JpfProtocol], with inits: FunctionBlocks? = nil, isTypeMember: Bool = false) -> JpfError? {
        for p in protocols {
            if let result = conform(to: p.clauses, with: inits, isTypeMember: isTypeMember), result.isError {return result}  // 準拠エラー
        }
        return nil
    }
    /// 規約(protocols)に違反していたら、エラーを返す。
    func conform(to protocols: [String], isTypeMember: Bool = false) -> JpfError? {
        for s in protocols {
            guard let p = self[s] as? JpfProtocol else {return ConformanceError.notFound(protocol: s).error}
            if let result = conform(to: p.clauses, isTypeMember: isTypeMember), result.isError {return result}  // 準拠エラー
        }
        return nil
    }
    /// 条項(clauses)に違反していたら、エラーを返す。
    private func conform(to clauses: [ClauseLiteral], with inits: FunctionBlocks? = nil, isTypeMember: Bool = false) -> JpfError? {
        for clause in clauses {
            guard isTypeMember == clause.isTypeMember else {continue}
            if clause.identifier.token.isKeyword(.INITIALIZATION) {
                if let error = conform(to: clause.signature!, with: inits!) {return error}
                continue
            }
            let name = clause.identifier.value, type = clause.type
            guard let target = self[name] else {return ConformanceError.definition(of: name + "(型：\(type))").error}
            guard type == target.type else {return ConformanceError.differentType(name: name, with: type).error}
            if let error = conform(to: clause, by: name, with: target) {return error}
        }
        return nil
    }
    /// 各条項(clause)に違反していたら、エラーを返す。
    private func conform(to clause: ClauseLiteral, by name: String, with target: JpfObject) -> JpfError? {
        switch target {
        case let function as JpfFunction:
            guard let signature = clause.signature else {break}     // 型(関数)のみチェック
            // 関数シグネチャのチェック
            if function.overload.hasSameSignature(as: signature) {break}// 関数シグネチャ準拠
            // エラー処理(関数シグネチャ準拠違反)
            guard let f = function.overload.array.last else {
                return ConformanceError.targetNotFound(function.type, in: function.name).error
            }
            return errorMessage(to: name, of: signature, with: f)
        case let computation as JpfComputation:
            // 算出シグネチャのチェック
            if clause.getterSignature == nil && clause.setterSignature == nil {break}   // 型(算出)のみチェック
            // 取得、設定が準拠しているか個別にチェック
            let isGetterConformed = clause.getterSignature.map {computation.getters.hasSameSignature(as: $0)} ?? true
            let isSetterConformed = clause.setterSignature.map {computation.setters.hasSameSignature(as: $0)} ?? true
            // 両方が準拠
            if isGetterConformed && isSetterConformed {break}
            // エラー処理(算出シグネチャ準拠違反)
            // 両方要求されているが、実装が両方とも無い場合
            if clause.getterSignature != nil && clause.setterSignature != nil &&
                computation.getters.array.isEmpty && computation.setters.array.isEmpty {
                return ConformanceError.targetNotFound("設定と取得", in: computation.name).error
            }
            // 取得が準拠していない場合のエラー
            if !isGetterConformed, let getSignature = clause.getterSignature {
                if let getter = computation.getters.array.last {
                    return errorMessage(to: name, of: getSignature, with: getter)
                } else {
                    return ConformanceError.targetNotFound("取得", in: computation.name).error
                }
            }
            // 設定が準拠していない場合のエラー
            if !isSetterConformed, let setSignature = clause.setterSignature {
                if let setter = computation.setters.array.last {
                    return errorMessage(to: name, of: setSignature, with: setter)
                } else {
                    return ConformanceError.targetNotFound("設定", in: computation.name).error
                }
            }
            assertionFailure(ConformanceError.targetNotFound(computation.type, in: computation.name).error.message)
            return ConformanceError.targetNotFound(computation.type, in: computation.name).error
        default:
            break
        }
        return nil
    }
    /// 初期化の準拠チェック
    private func conform(to signature: FunctionSignature, with inits: FunctionBlocks) -> JpfError? {
        if inits.hasSameSignature(as: signature) {return nil}
        // エラー処理(初期化シグネチャ準拠違反)
        guard let i = inits.array.last else {
            return ConformanceError.targetNotFound(JpfType.type, in: TypeLiteral.syokika).error
        }
        return errorMessage(to: TypeLiteral.syokika, of: signature, with: i)
    }
    /// 入出力のエラーを検出
    private func errorMessage(to ident: String, of s: FunctionSignature, with f: FunctionBlock) -> JpfError {
        // 引数が一致しない
        guard s.parameters.count == f.parameters.count else {
            return s.parameters.isEmpty ?
            ConformanceError.unexpectedInputDefinition(in: ident).error :      // 入力定義が不要
            ConformanceError.numberOfParams(s.parameters.count, to: ident).error
        }
        if let mismatch = zip(s.parameters, f.parameters).first(where: {$0.value != $1.value}) {
            return ConformanceError.nameOfParams(mismatch.0.value, to: ident).error
        }
        // 入力形式が一致しない
        if let err = compareParamForm(s.paramForm, to: f.paramForm, in: ident) {return err}
        // 返り値の型が一致しない
        if let err = compareReturnTypes(s.returnTypes, to: f.returnTypes, in: ident) {return err}
        assertionFailure(ConformanceError.undefined.error.message)
        return ConformanceError.undefined.error
    }
    /// 入力形式が一致しなければ、エラーを返す。
    private func compareParamForm(_ lhs: InputFormat, to rhs: InputFormat, in ident: String) -> JpfError? {
        // 入力形式の数が一致しない
        guard lhs.formats.count == rhs.formats.count else {
            assertionFailure(ConformanceError.numberOfParams(lhs.formats.count, to: ident).error.message)   // 引数の数は一致しているので、エラーにならない
            return ConformanceError.numberOfParams(lhs.formats.count, to: ident).error
        }
        // 入力の形式が一致しない
        if let mismatch = zip(lhs.formats, rhs.formats).first(where: {
            $0.type != $1.type || $0.particle != $1.particle || $0.threeDots != $1.threeDots
        }) {
            return ConformanceError.formatOfParams("\(mismatch.0.type)\(mismatch.0.particle)\(mismatch.0.threeDots)", in: ident).error
        }
        return nil
    }
    /// 返り値の型をチェック
    private func compareReturnTypes(_ lhs: [String]?, to rhs: [String], in ident: String) -> JpfError? {
        guard let target = lhs else {return ConformanceError.undefined.error}
        // 返り値の数が不一致
        guard target.count == rhs.count else {
            return target.isEmpty ?
            ConformanceError.unexpectedOutputDefinition(in: ident).error :     // 出力定義が不要
            ConformanceError.numberOfReturns(target.count, to: ident).error
        }
        if let mismatch = zip(target, rhs).first(where: { $0 != $1 }) {
            return ConformanceError.typeOfReturns("\(mismatch.0)", in: ident).error
        }
        return nil
    }
    /// 出力値の型をチェック
    /// - Parameters:
    ///     - types: チェックする型の配列
    ///       1. <型名>
    ///       2. <型名>か無
    ///       3. <型名>かエラー
    ///       4. 「」(空)
    ///     - result: スタックに無い返り値
    /// - Returns: エラーが無ければnil、あればJpfError
    func errorMessage(to types: [String], with result: JpfObject? = nil) -> JpfError? {
        guard !types.isEmpty else { return nil }        // チェック不要
        // 「」(空)のチェック
        if types.contains(where: \.isEmpty) {
            guard types.count == 1 else {               // 形式チェック
                assertionFailure(ReturnTypesError.illeagalEmptyFormat.message.message)  // Parserエラーなので、ここではあり得ない。
                return ReturnTypesError.illeagalEmptyFormat.message
            }
            guard result == nil && isEmpty else {       // 期待しない出力のチェック
                let result = result?.isReturnValue == true ? result!.value : result
                return ReturnTypesError.unexpectedOutput(result?.type ?? peek!.type).message
            }
            return nil
        }
        // 出力が「自身(か…)」
        if types.count == 1 && types[0].contains(JpfInstance.SELF) {
            return errorMessage(to: types[0], with: result)
        }
        // 出力値を取得、形成
        let hasReturnValue = (result?.isReturnValue == true && result?.hasValue == true) || result?.isError == true
        let n = types.count - (hasReturnValue ? 1 : 0)
        guard var values = peek(n) else {               // スタックから出力を得る
            return ReturnTypesError.numberOfTypes(types.count).message  // 足りない
        }
        if hasReturnValue, let value = result?.value  { // ReturnValueが値を持つ、またはエラー
            values.append(value)
        }
        // 各値を型チェック
        return errorMessage(to: types, with: values)
    }
    /// 初期化の出力型チェック
    private func errorMessage(to type: String, with result: JpfObject?) -> JpfError? {
        guard !isEmpty || result != nil else {return nil}   // 自身を返す
        let type = type.dropFirst(JpfInstance.SELF.count).replacingOccurrences(of: "か", with: "")
        return errorMessage(to: [type], with: result)
    }
    /// 個々の値の型をチェック
    private func errorMessage(to types: [String], with values: [JpfObject]) -> JpfError? {
        for (type, value) in zip(types, values) {
            if let error = errorMessage(to: type, with: value) {
                return error
            }
        }
        return nil
    }
    /// 値の型が一致するかチェック
    /// - Parameters:
    ///   - type: <型名>、<型名>か無、<型名>かエラー
    ///   - value: 値
    /// - Returns: 型が一致ならばnil、不一致ならばエラー
    private func errorMessage(to type: String, with value: JpfObject) -> JpfError? {
        type.split(separator: "か")
            .map(String.init)
            .first(where: { value.contains(type: $0) }) == nil
            ? ReturnTypesError.type(value.type).message
            : nil
    }
}
// MARK: - エラーメッセージ
enum InputFormatError : Error {
    case numberOfParameters(Int)
    case type(String)
    case particle(String)
    case particleFormat(String)
    case variable(String)
    case notFoundFirstFormat
    case noMatchingSignature
    case noParameterValue
    case failedToEvaluate
    case typeOfDefaultValue
    case cannotGetDefault
    /// エラーメッセージ
    var message: JpfError {
        switch self {
        case .numberOfParameters(let number):   return JpfError("入力の数が足りていない。必要数：\(number)")
        case .type(let type):                   return JpfError("入力の型が異なる。入力の型：\(type)")
        case .particle(let particle):           return JpfError("入力の格が異なる。入力の格：\(particle)")
        case .particleFormat(let s):            return JpfError("格の形式が誤っている。指定：\(s)")
        case .variable(let type):               return JpfError("可変長識別子の値が配列ではない。型：\(type)")
        case .notFoundFirstFormat:              return JpfError("可変長識別子に既定値が設定されている。")
        case .noMatchingSignature:              return JpfError("入力形式が一致する関数が見つからなかった。")
        case .noParameterValue:                 return JpfError("固定長パラメータの値が取得できなかった。")
        case .failedToEvaluate:                 return JpfError("既定値の評価に失敗した。")
        case .typeOfDefaultValue:               return JpfError("既定値の型が指定形式と一致しない。")
        case .cannotGetDefault:                 return JpfError("既定値が設定されていなかった。")
        }
    }
}
enum ReturnTypesError : Error {
    case numberOfTypes(Int)
    case type(String)
    case illeagalEmptyFormat
    case unexpectedOutput(String)
    var message: JpfError {
        switch self {
        case .numberOfTypes(let number):        return JpfError("出力の数が足りていない。必要数：\(number)")
        case .type(let type):                   return JpfError("出力の型が異なる。実際の型：\(type)")
        case .illeagalEmptyFormat:              return JpfError("「」の指定形式が間違っている。使い方：出力が「」")
        case .unexpectedOutput(let type):       return JpfError("期待しない出力。型：\(type)")
        }
    }
}
/// 準拠違反
enum ConformanceError : Error {
    case notFound(protocol: String)
    case duplicated(protocol: String)
    case definition(of: String)
    case differentType(name: String, with: String)
    case unexpectedInputDefinition(in: String)
    case numberOfParams(Int, to: String)
    case nameOfParams(String, to: String)
    case formatOfParams(String, in: String)
    case numberOfReturns(Int, to: String)
    case typeOfReturns(String, in: String)
    case unexpectedOutputDefinition(in: String)
    case targetNotFound(String, in: String)
    case undefined                              // 未定義エラー
    /// エラーメッセージ
    var error: JpfError {
        switch self {
        case .notFound(let s):          return JpfError("準拠する規約「\(s)」が見つからない。")
        case .duplicated(let s):        return JpfError("準拠する規約「\(s)」の定義が重複している。")
        case .definition(let s):        return JpfError("規約に準拠するためには、「\(s)」の定義が必要。")
        case .differentType(let i, let t):
                                        return JpfError("規約に準拠するためには、「\(i)」を、型「\(t)」で定義する必要がある。")
        case .unexpectedInputDefinition(let i):
                                        return JpfError("「\(i)」には、入力定義は不要。")
        case .numberOfParams(let n, let i):
                                        return JpfError("規約に準拠するためには、「\(i)」の入力定義に\(n)個の引数が必要。")
        case .nameOfParams(let s, let i):
                                        return JpfError("規約に準拠するためには、「\(i)」に引数「\(s)」が必要。")
        case .formatOfParams(let f, let i):
                                        return JpfError("規約に準拠するためには、「\(i)」に入力形式「\(f)」が必要。")
        case .numberOfReturns(let n, let i):
                                        return JpfError("規約に準拠するためには、「\(i)」の出力定義に\(n)個の型が必要。")
        case .typeOfReturns(let t, let i):
                                        return JpfError("規約に準拠するためには、「\(i)」に出力型「\(t)」が必要。")
        case .unexpectedOutputDefinition(let i):
                                        return JpfError("「\(i)」には、出力定義は不要。")
        case .targetNotFound(let f, let i):
                                        return JpfError("規約に準拠するためには、「\(i)」に「\(f)」の定義が必要。")
        case .undefined:                return JpfError("未定義の規約準拠違反。")
        }
    }
}
