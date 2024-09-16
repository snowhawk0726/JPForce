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
        case .keyword(.EQUAL),.keyword(.BE),.keyword(.NOT):
                                    return BooleanOperationCompiler(compiler, by: token)
        case .keyword(.LT),.keyword(.GT):
                                    return CompareOperationCompiler(compiler, by: token)
        case .keyword(.SURU):       return PerformCompiler(compiler)    // 〜にする、〜をする
        default:                    return nil
        }
    }
}
// MARK: - predicate compilable implements
extension PredicateCompilable {
    var isPeekNumber: Bool {compiler.peek?.isNumber ?? false}
    func isPeekParticle(_ p: Token.Particle) -> Bool {
        compiler.peek?.particle.map {$0.type} == Token.particle(p).type
    }
    var unwrappedPeek: JpfObject? {
        if compiler.peek is JpfPhrase {return compiler.peek?.value}
        return compiler.peek
    }
    var leftOperand: JpfObject? {return compiler.pull()}
    var leftNumber: Int? {
        guard let number = compiler.peek?.number else {return nil}
        compiler.drop()
        return number
    }
    // エラー
    var additionParamError: JpfError    {JpfError("「足す」には、２つ以上の数値、文字列、配列入力が必要。")}
    var numerationParamError1: JpfError {JpfError("には２つの数値入力が必要。")}
    var numerationParamError2: JpfError {JpfError("には２つ以上の数値入力が必要。")}
    var numerationParamError3: JpfError {JpfError("には１つの数値入力が必要。")}
    var atLeastOneParamError: JpfError  {JpfError("には１つ以上の入力が必要。")}
    var oneParamNeeded: JpfError        {JpfError("には１つの入力が必要。")}
    var twoParamsNeeded: JpfError       {JpfError("には２つの入力が必要。")}
    var cannotDivideByZero: JpfError    {JpfError("0で割ることはできない。")}
    var valueNotFound: JpfError         {JpfError("で判定すべき値が無かった。")}
    var rangeFormatError: JpfError      {JpfError("範囲の判定対象は数値のみ。")}
    var particleError: JpfError         {JpfError("助詞が間違っている。")}
    var determineError: JpfError        {JpfError("判定の述語が間違っている。述語：「ある」「ない」「等しい」")}
    var cannotCompare: JpfError         {JpfError("では比較できない。")}
    var cannotJudgeGenuineness: JpfError{JpfError("で、正負を判定できる対象は、数値型のみ。")}
    // 使い方
    var additionUsage: JpfError         {JpfError("仕様：(〜と…)〜を足す。")}
    var multiplicationUsage: JpfError   {JpfError("仕様：(〜と…)〜を掛ける。")}
    var substractionUsage: JpfError     {JpfError("仕様：(〜から)〜を引く。または、(〜を)〜から引く。")}
    var divisionUsage: JpfError         {JpfError("仕様：(〜を)〜で割る。または、(〜で)〜を割る。")}
    var negateUsage: JpfError           {JpfError("仕様：〜の負数。または、〜を負数にする。")}
    var determineUsage: JpfError        {JpfError("仕様：〜が<配列、範囲>に")}
    var rangeCheckUsage: JpfError       {JpfError("仕様：<数値>が範囲【<範囲式>】に")}
    var beUsage: JpfError               {JpfError("仕様：(〜が)〜である。または、(〜は)〜である。")}
    var notUsage: JpfError              {JpfError("仕様：(〜が)〜で(は)ない。または、(〜は)〜で(は)ない。")}
    var equalUsage: JpfError            {JpfError("仕様：〜(と)〜(が)等しい。")}
}
// MARK: - 算術演算
struct AddCompiler : PredicateCompilable {
    init(_ compiler: Compiler) {self.compiler = compiler}
    let compiler: Compiler
    func compiled() -> JpfObject? {
        guard let params = compiler.peek(2) else {return additionParamError + additionUsage}
        var added = params[0].add(params[1])
        if !added.isError {compiler.drop(2)}
        while isPeekParticle(.TO) {     // スタックにト格があれば、中身を足す
            added = leftOperand!.add(added)
            if added.isError {return added}
        }
        return added
    }
}
struct MultiplyCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        guard let params = compiler.peek(2),
              let left = params[0].number, let right = params[1].number else {return "「\(op.literal)」" + numerationParamError2 + multiplicationUsage}
        let ident = params[0].value?.name ?? ""
        var number = left * right
        compiler.drop(2)
        while isPeekParticle(.TO) && isPeekNumber { // スタックにト格の数値があれば、掛ける
            number *= leftNumber!
        }
        return JpfInteger(name: ident, value: number)
    }
}
struct SubstractCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        guard let params = compiler.peek(2),
              let left = params[0].number, let right = params[1].number else {return "「\(op.literal)」" + numerationParamError1 + substractionUsage}
        let ident = params[0].value?.name ?? ""
        var integer: JpfInteger
        switch (params[0].particle, params[1].particle) {
        case (Token(.KARA),Token(.WO)), (nil,Token(.WO)), (nil,nil):    // leftから、rightを引く
            compiler.drop(2)
            integer = JpfInteger(name: ident, value: left - right)
        case (Token(.WO),Token(.KARA)), (nil,Token(.KARA)):             // leftを、rightから引く
            compiler.drop(2)
            integer = JpfInteger(name: ident, value: right - left)
        default:
            return substractionUsage
        }
        return integer
    }
}
struct DivideCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        guard let params = compiler.peek(2),
              let left = params[0].number, let right = params[1].number else {return "「\(op.literal)」" + numerationParamError1 + divisionUsage}
        let ident = params[0].value?.name ?? ""
        switch (params[0].particle, params[1].particle) {
        case (Token(.WO),Token(.DE)), (nil,Token(.DE)), (nil,nil):      // leftを、rightで割る
            guard right != 0 else {return "\(left)を" + cannotDivideByZero}
            compiler.drop(2)
            return JpfInteger(name: ident, value: left / right)
        case (Token(.DE),Token(.WO)), (nil,Token(.WO)):                 // leftで、rightを割る
            guard left != 0 else {return "\(right)を" + cannotDivideByZero}
            compiler.drop(2)
            return JpfInteger(name: ident, value: right / left)
        default:
            return divisionUsage
        }
    }
}
struct NegateCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        guard let number = compiler.peek?.number else {return "「\(op.literal)」" + numerationParamError3 + negateUsage}
        let ident = compiler.peek!.value?.name ?? ""
        compiler.drop()
        return JpfInteger(name: ident, value: -number)
    }
}
struct SignCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        let particle = compiler.peek?.particle
        if let number = compiler.unwrappedPeek as? JpfInteger {
            compiler.drop()
            return number[op.literal, particle]
        }
        return "「\(op.literal)」" + cannotJudgeGenuineness
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
        guard let object = unwrappedPeek else {return "「\(op.literal)」" + oneParamNeeded}
        compiler.drop()
        return object
    }
}
// MARK: - 比較演算
struct BooleanOperationCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        if let params = compiler.peek(3),
           params[1] is JpfRange && params[2].value is JpfRange {   // 上限と下限に分かれた範囲をマージする。
            guard let range = mergeRanges(params[1], with: params[2]) else {
                return determineUsage + "\(op.literal)。"
            }
            compiler.drop(2)
            compiler.push(range)
        }
        if let params = compiler.peek(2) {                   // 入力が２つ
            switch (params[0].particle, params[1].particle, op.type) {
            case (.particle(.GA),.particle(.DE),.keyword(.BE)),(.particle(.WA),.particle(.DE),.keyword(.BE)),
                (.particle(.GA),.particle(.DE),.keyword(.NOT)),(.particle(.WA),.particle(.DE),.keyword(.NOT)),
                (.particle(.GA),.particle(.DEWA),.keyword(.NOT)),(.particle(.WA),.particle(.DEWA),.keyword(.NOT)),
                (_,_,.keyword(.EQUAL)):                         // 当否判定
                guard let left = params[0].value, let right = params[1].value else {
                    return "「\(op.literal)」:" + valueNotFound + ": \(params[0])または\(params[1])"
                }
                compiler.drop(2)
                return determined(left, op.type, right)
            case (.particle(.GA),.particle(.NI),.keyword(.BE)),(.particle(.WA),.particle(.NI),.keyword(.BE)),
                (.particle(.GA),.particle(.NI),.keyword(.NOT)),(.particle(.WA),.particle(.NI),.keyword(.NOT)): // 有無判定(含む)
                guard let left = params[0].value, let right = params[1].value else {return determineUsage + "\(op.literal)。"}
                switch right {
                case is JpfRange:
                    guard left.isNumber else {return rangeFormatError + rangeCheckUsage  + "\(op.literal)。"}
                case is JpfArray:
                    break
                default:
                    return determineUsage + "\(op.literal)。"
                }
                compiler.drop(2)
                return determined(left, op.type, right)
            case (_,_,.keyword(.BE)),(_,_,.keyword(.NOT)):      // 当否判定
                compiler.drop()
                return determined(params[1], op.type)
            default:
                return "「\(op.literal)」:" + particleError + (op.type == .keyword(.BE) ? beUsage : notUsage)
            }
        } else
        if let operand = compiler.peek {                     // 入力が１つ
            compiler.drop()
            if op.type == .keyword(.EQUAL) {
                operand.emit(with: compiler)
                _ = compiler.emit(op: .opEqual)
                return nil
            }
            return determined(operand, op.type)                 // 当否判定
        }
        if op.type == .keyword(.NOT) {_ = compiler.emit(op: .opNot)}
        return nil
    }
    private func determined(_ left: JpfObject, _ opType: Token.TokenType, _ right: JpfObject) -> JpfObject {
        if !(left is JpfArray && opType == .keyword(.EQUAL)) &&
            !(left is JpfRange && opType == .keyword(.EQUAL)) {
            if let array = right as? JpfArray {
                let result = array.contains(left)
                return determined(result, opType)
            }
            if let range = right as? JpfRange {
                let result = range.contains(left)
                guard !result.isError else {return result} // 範囲の形式エラー
                return determined(result, opType)
            }
        }
        switch opType {
        case .keyword(.BE),
             .keyword(.EQUAL):  return JpfBoolean.object(of: left.isEqual(to: right))
        case .keyword(.NOT):    return JpfBoolean.object(of: !left.isEqual(to: right))
        default:                return determineError
        }
    }
    private func determined(_ operand: JpfObject, _ opType: Token.TokenType) -> JpfObject {
        switch opType {
        case .keyword(.BE),
             .keyword(.EQUAL):  return JpfBoolean.object(of: operand.isTrue)
        case .keyword(.NOT):    return JpfBoolean.object(of: !operand.isTrue)
        default:                return determineError
        }
    }
    private func mergeRanges(_ o1: JpfObject, with o2: JpfObject) -> JpfPhrase? {
        guard let r = o1 as? JpfRange, let lower = r.lowerBound else {return nil}
        guard let p = o2 as? JpfPhrase,
              let r = p.value as? JpfRange, let upper = r.upperBound else {return nil}
        return JpfPhrase(value: JpfRange(lowerBound: lower, upperBound: upper), particle: p.particle)
    }
}
struct CompareOperationCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        guard let params = compiler.peek(2),
              let left = params[0].number, let right = params[1].number else {return "「\(op.literal)」" + numerationParamError1}
        guard let result = compared(left, op.type, right) else {return "「\(left)」と「\(right)」は、「\(op.literal)」" + cannotCompare}
        compiler.drop(2)
        return JpfBoolean.object(of: result)
    }
    private func compared(_ left: Int, _ opType: Token.TokenType, _ right: Int) -> Bool? {
        switch opType {
        case .keyword(.LT):         return left < right
        case .keyword(.GT):         return left > right
        default:
            return nil
        }
    }
}
// MARK: -
struct PerformCompiler : PredicateCompilable {
    init(_ compiler: Compiler) {self.compiler = compiler}
    let compiler: Compiler
    func compiled() -> JpfObject? {
        switch compiler.unwrappedPeek {
//        case let function as JpfFunction:
//            compiler.drop()
//            return function.executed(with: compiler)
        case let value?:
            compiler.drop()
            return value
        default:
            return nil
        }
    }
}
