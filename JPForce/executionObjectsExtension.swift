//
//  executionObjectsExtension.swift
//  インタープリタでの実行/生成/初期化を実現するJpfObjectの拡張
//  JPForce
//
//  Created by 佐藤貴之 on 2025/12/27.
//

///  Object Extentions
extension JpfFunction {
    /// 関数を実行する。
    /// スタックの引数は、関数ローカルの辞書に登録される。(local.apply())
    /// 本体は評価実行され、エラーかアンラップされた返り値を返す。
    /// - Parameter environment: 実行中の(通常もしくは関数の)環境
    /// - Returns: エラーかアンラップされた返り値、なければnil
    func execute(with environment: Environment) -> JpfObject? {
        let local = Environment(outer: self.environment)    // 関数の環境を拡張
        let stackEnv = environment.isEmpty ? self.environment : environment
        defer {_ = environment.push(stackEnv.pullAll())}    // スタックを戻す
        return stackEnv.execute(overload, with: local)
    }
}
extension JpfComputation {
    /// 算出(取得)を行う。
    /// - Parameter environment: 実行中の(通常もしくは算出の)環境
    /// - Returns: エラーかアンラップされた返り値、なければnil
    func getter(with environment: Environment) -> JpfObject? {
        guard !getters.isEmpty else {return getterNotFound}
        let local = Environment(outer: self.environment)    // 環境を拡張
        return environment.execute(getters, with: local) ?? environment.pull()  // 返り値が無ければスタックから値を取る。
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
extension JpfType {
    /// インスタンスを生成する。
    /// - Parameter outer: 生成元の環境
    /// - Returns: インスタンス、もしくはエラー
    func create(with outer: Environment) -> JpfObject {
        let local = Environment(outer: outer)               // 型の環境を拡張
        defer {_ = outer.push(local.pullAll())}             // スタックを戻す
        var protocols: [JpfProtocol] = []                   // 規約(型)登録
        for ident in self.protocols {
            if let rule = outer[ident] as? JpfProtocol {
                protocols.append(rule)
                if let implements = rule.body {             // 規約のデフォルト実装をlocalに登録
                    if let result = Evaluator(from: implements, with: local).object, result.isError {return result}
                }
            }
        }
        if let body = body,                                 // 定義ブロック
           let result = Evaluator(from: body, with: local).object, result.isError {return result}   // メンバ登録
        if let result = local.conform(to: protocols, with: initializers), result.isError {return result}                // 規約チェック
        var members = (local.peek as? JpfArray).map {$0.elements.compactMap {$0 as? JpfString}.map {$0.value}} ?? []  // 利用可能なメンバーリスト
        local.drop()
        for p in protocols {    // 規約条項のメンバーリストを利用可能なメンバーリストに追加
            p.clauses.forEach {members.append($0.identifier.value)}
        }
        return JpfInstance(type: self.name, environment: local, protocols: self.protocols, available: Set(members))
    }
}
extension JpfInstance {
    /// 生成、初期化から、インスタンスの初期化を行う。
    /// - Parameter outer: スタックによる引数を含む環境
    /// - Returns: 成功: nil、失敗: 無(JpfNull)、またはエラー(JpfError)
    func initialize(with outer: Environment) -> JpfObject? {
        guard let type = outer[self.type] as? JpfType else {return JpfError(typeNotFound)}
        return initialize(type: type, outer: outer) {inits, env in
            outer.execute(inits, with: env)
        }
    }
    /// 呼び出し式から、インスタンスの初期化を行う。
    /// - Parameter type: 生成元の型
    /// - Returns: 成功: nil、失敗: 無(JpfNull)、またはエラー(JpfError)
    func initialize(with type: JpfType) -> JpfObject? {
        guard let outer = environment.outer else {return nil}
        return initialize(type: type, outer: outer) {inits, env in
            outer.call(inits, with: env)
        }
    }
    /// 共通処理
    private func initialize(type: JpfType, outer: Environment, callee: (FunctionBlocks, Environment) -> JpfObject?) -> JpfObject? {
        environment[JpfInstance.SELF] = self            // 自身を辞書に登録
        if type.initializers.isEmpty {return nil}       // 定義がない場合は、なにもしない
        if let result = callee(type.initializers, environment) {
            if result.isError {return result.error}     // エラーを返す
            if result.isReturnValue {return result.value}
            return result
        }
        if let result = outer.peek, result.isNull {     // スタックの無を返り値として返す。
            return outer.pull()
        }
        return nil
    }
}
