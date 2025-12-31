//
//  functionBlocks.swift
//　多重定義の管理
//  JPForce
//
//  Created by 佐藤貴之 on 2025/12/26.
//

import Foundation

// MARK: - 定義・初期化
/// 必要入力数ごとの関数ブロック配列(多重定義)
final class FunctionBlocks {
    private static let variadicArity = -1
    private var dictionary: [Int : [FunctionBlock]] = [:]   // 引数数毎の関数ブロック
    private var array: [FunctionBlock] = []                 // 関数ブロック定義順
    private var keyword: Token.Keyword? = nil               // 予約語多重定義
    //
    init() {}
    init(by keyword: Token.Keyword) {self.keyword = keyword}
    /// 関数定義で初期化
    init(_ functionBlock: FunctionBlock) {_ = self.append(functionBlock)}
}
// MARK: - アクセス・インタフェース
extension FunctionBlocks {
    /// 関数定義を多重定義(自身)に追加する。
    /// - Parameter functionBlock: 追加する関数ブロック (isOverloaded==falseならば上書き)
    /// - Returns: 追加した自身を返す
    func append(_ functionBlock: FunctionBlock) -> Self {
        functionBlock.rangeOfInputs.forEach {
            register(functionBlock, for: $0)
        }
        if array.isEmpty || functionBlock.isOverloaded {
            array.append(functionBlock)
        }
        return self
    }
    /// 多重定義を多重定義(自身)に追加する。
    /// - Parameter functionBlocks: 追加する多重定義 (hasRedefineならば上書き)
    /// - Returns: 追加した自身を返す
    func append(_ functionBlocks: FunctionBlocks) -> Self {
        if functionBlocks.hasRedefinition {
            applyRedefinition(from: functionBlocks)
        } else {
            applyOverloads(from: functionBlocks)
        }
        return self
    }
    /// 必要入力数毎の多重定義配列を返す。(無い場合は空)
    subscript(index: Int) -> [FunctionBlock] {dictionary[index] ?? []}
    // 要素アクセス
    var all: [FunctionBlock] {array}
    var count: Int {array.count}
    var isEmpty: Bool {array.isEmpty}
    var hasDefinitions: Bool {!array.isEmpty}
    /// 最新定義を取得する
    var latest: FunctionBlock? {array.last}
    var single: FunctionBlock? {array.first}
    /// 単体定義かどうか
    var isSingleDefinition: Bool {array.count == 1}
    /// 指定入力数を受け付ける可能性があるか
    func accepts(numberOfInputs n: Int) -> Bool {
        dictionary.keys.contains(n) ||
        dictionary.keys.contains(Self.variadicArity)
    }
    @discardableResult
    func markAsOverloaded() -> Self {
        self.array.forEach {$0.isOverloaded = true}
        return self
    }
    /// 再定義している(多重定義でない関数ブロックが含まれる)
    var hasRedefinition: Bool {array.contains {!$0.isOverloaded}}
    /// 多重定義に、指定のシグネチャと一致する関数ブロックがあるか？
    /// - Parameter signature: 関数シグネチャ
    /// - Returns: シグネチャが一致ならば、true
    func hasSameSignature(for signature: FunctionSignature) -> Bool {
        array.reversed().contains {$0.matches(for: signature)}
    }
    /// 入力と引数の形式が一致する関数ブロックを得る。(call expression用)
    /// - Parameter env: 引数をもつ環境
    /// - Returns: 関数ブロック(無ければnil)
    func resolve(with env: Environment) -> FunctionBlock? {
        let pairs = env.parameterPairs          // 引数の名前(k)と値(v)の組
        let vFunctions = dictionary[Self.variadicArity] // 可変長の定義
        let fFunctions = dictionary[pairs.count]// 固定長の定義
        switch (vFunctions, fFunctions) {
        case let (v?, f?):                      // 両方チェック
            if let function = match(in: v, with: pairs) {return function}
            return match(in: f, with: pairs)
        case let (f?, nil), let (nil, f?):      // 片方チェック
            return match(in: f, with: pairs)
        default:
            return nil
        }
    }
}
// MARK: - 述語の再定義
extension FunctionBlocks {
    @discardableResult
    func redefinePredicate(by keyword: Token.Keyword) -> Self {
        self.keyword = keyword
        return self
    }
    var isRedefinedPredicate: Bool {keyword != nil}
    func operateBuiltinPredicate(with env: Environment) -> JpfObject? {
        guard let keyword else {
            preconditionFailure("operateBuiltinPredicate called without keyword")
        }
        let predicate = PredicateOperableFactory.create(from: keyword, with: env)
        return predicate.operate()
    }
}
// MARK: - サブルーチン
private extension FunctionBlocks {
    /// 候補の関数ブロック配列から、指定引数(名前と値)形式を持つ関数ブロックを返す。
    /// - Parameters:
    ///   - functions: 候補の関数ブロック配列
    ///   - pairs: 指定引数(名前と値)
    /// - Returns: 関数ブロック(見つからなければnil)
    func match(in functions: [FunctionBlock], with pairs: [(String, JpfObject)]) -> FunctionBlock? {
        functions: for f in functions.reversed() {
            arguments: for (k, v) in pairs {
                if f.hasVariableParameter(named: k) {// 可変長識別子の値の型は配列
                    guard v.type == JpfArray.type else {continue functions}
                } else {                            // 固定長識別子の値の型は引数の型
                    guard f.hasParameter(named: k,  ofType: v.type) else {continue functions}
                }
            }
            return f                                // 引数名と型が全て一致
        }
        return nil                                  // 一致する関数が無い
    }
    func register(_ block: FunctionBlock, for arity: Int) {
        if dictionary[arity] != nil && block.isOverloaded {
            dictionary[arity]!.append(block)
        } else {
            dictionary[arity] = [block]
        }
    }
    func applyRedefinition(from other: FunctionBlocks) {
        for (arity, blocks) in other.dictionary {
            dictionary[arity] = blocks
        }
        array = other.array
    }
    func applyOverloads(from other: FunctionBlocks) {
        for (arity, blocks) in other.dictionary {
            if dictionary[arity] != nil {
                dictionary[arity]! += blocks
            } else {
                dictionary[arity] = blocks
            }
        }
        array += other.array
    }
}
