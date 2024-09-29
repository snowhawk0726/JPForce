//
//  predicateCompilable.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/09/07.
//

import Foundation

// MARK: - interfaces for predicate compilation
protocol PredicateCompilable {
    var compiler: Compiler {get}
    /// 述語を翻訳し、バイトコードを出力(emit)する。
    /// 定数で演算可能な場合、演算しCompilerにキャッシュ(push)する。
    /// - Returns: 
    ///   翻訳済みの場合、nilを返す。
    ///   エラーを検出した場合、JpfErrorを返す。
    ///   キャッシュによる演算が継続可能な場合、値(JpfObject)を出力する。
    func compiled() -> JpfObject?
}
// MARK: - predicate compilable instance factory
struct PredicateCompilableFactory {
    static func create(from token: Token, with compiler: Compiler) -> PredicateCompilable? {
        switch token.type {
        case .keyword(.ADD):        return AddCompiler(compiler)
        case .keyword(.MULTIPLY):   return MultiplyCompiler(compiler, by: token)
        case .keyword(.SUBSTRACT):  return SubstractCompiler(compiler, by: token)
        case .keyword(.DIVIDE):     return DivideCompiler(compiler, by: token)
        case .keyword(.NEGATE):     return NegateCompiler(compiler, by: token)
        case .keyword(.POSITIVE),.keyword(.NEGATIVE):
                                    return SignCompiler(compiler, by: token)
        case .keyword(.MONO):       return UnwrapCompiler(compiler, by: token)
        case .keyword(.BE):         return BeOperationCompiler(compiler)
        case .keyword(.NOT):        return NotOperationCompiler(compiler)
        case .keyword(.EQUAL):      return EqualOperationCompiler(compiler)
        case .keyword(.LT),.keyword(.GT):
                                    return CompareOperationCompiler(compiler, by: token)
        case .keyword(.SURU):       return PerformCompiler(compiler)    // 〜にする、〜をする
        default:                    return nil
        }
    }
}
// MARK: - predicate compilable implements
extension PredicateCompilable {
}
// MARK: - 算術演算
struct AddCompiler : PredicateCompilable {
    init(_ compiler: Compiler) {self.compiler = compiler}
    let compiler: Compiler
    func compiled() -> JpfObject? {
        if compiler.count < 2 {
            if let object = compiler.unwrappedObject() {
                object.emit(with: compiler)
            }
            _ = compiler.emit(op: .opAdd)
            return nil
        }
        return AddOperator(compiler.environment).operated()
    }
}
struct MultiplyCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        if compiler.count < 2 {
            if let object = compiler.unwrappedObject() {
                object.emit(with: compiler)
            }
            _ = compiler.emit(op: .opMul)
            return nil
        }
        return MultiplyOperator(compiler.environment, by: op).operated()
    }
}
struct SubstractCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        if compiler.count < 2 {
            if let object = compiler.unwrappedObject() {
                object.emit(with: compiler)
            }
            _ = compiler.emit(op: .opSub)
            return nil
        }
        return SubstractOperator(compiler.environment, by: op).operated()
    }
}
struct DivideCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        if compiler.count < 2 {
            if let object = compiler.unwrappedObject() {
                object.emit(with: compiler)
            }
            _ = compiler.emit(op: .opDiv)
            return nil
        }
        return DivideOperator(compiler.environment, by: op).operated()
    }
}
struct NegateCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        if compiler.isEmpty {
            _ = compiler.emit(op: .opNeg)
            return nil
        }
        return NegateOperator(compiler.environment, by: op).operated()
    }
}
struct SignCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        if compiler.isEmpty {
            _ = compiler.emit(op: .opConstant, operand: compiler.addConstant(JpfInteger(value: 0)))
            _ = op.isKeyword(.POSITIVE) ?
                compiler.emit(op: .opGreaterThan) :
                compiler.emit(op: .opLessThan)
            return nil
        }
        return SignOperator(compiler.environment, by: op).operated()
    }
}
// MARK: - 補助演算
/// <式>たもの → <式>
/// 「もの」(Keyword(.MONO))は、入力から値(式)を取り出す。
/// 取り出された値は、続く助詞を付けた句としてスタックに返される。
/// 例： 10を5で割ったものに　→ (2に)、2を二倍したものを → (4を)
struct UnwrapCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        return UnwrapOperator(compiler.environment, by: op).operated()
    }
}
// MARK: - 比較演算
struct BeOperationCompiler : PredicateCompilable {
    init(_ compiler: Compiler) {self.compiler = compiler}
    let compiler: Compiler
    func compiled() -> JpfObject? {
        if compiler.count < 2 {
            if let value = compiler.pull() {
                value.emit(with: compiler)
            }
            _ = compiler.emit(op: .opBe)
            return nil
        }
        return BooleanOperator(compiler.environment, by: .keyword(.BE)).operated()
    }
}
struct NotOperationCompiler : PredicateCompilable {
    init(_ compiler: Compiler) {self.compiler = compiler}
    let compiler: Compiler
    func compiled() -> JpfObject? {
        if compiler.count < 2 {
            if let value = compiler.pull() {
                value.emit(with: compiler)
            }
            _ = compiler.emit(op: .opNot)
            return nil
        }
        return BooleanOperator(compiler.environment, by: .keyword(.NOT)).operated()
    }
}
struct EqualOperationCompiler : PredicateCompilable {
    init(_ compiler: Compiler) {self.compiler = compiler}
    let compiler: Compiler
    func compiled() -> JpfObject? {
        if compiler.count < 2 {
            if let value = compiler.pull() {
                value.emit(with: compiler)
            }
            _ = compiler.emit(op: .opEqual)
            return nil
        }
        return BooleanOperator(compiler.environment, by: .keyword(.EQUAL)).operated()
    }
}
struct CompareOperationCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        if compiler.count < 2 {
            if let value = compiler.pull() {
                value.emit(with: compiler)
            }
            _ = op.isKeyword(.GT) ?
                compiler.emit(op: .opGreaterThan) :
                compiler.emit(op: .opLessThan)
            return nil
        }
        return CompareOperator(compiler.environment, by: op).operated()
    }
}
// MARK: -
struct PerformCompiler : PredicateCompilable {
    init(_ compiler: Compiler) {self.compiler = compiler}
    let compiler: Compiler
    func compiled() -> JpfObject? {
        return PerformOperator(compiler.environment).operated()
    }
}
