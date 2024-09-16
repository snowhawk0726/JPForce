//
//  compiler.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/06/19.
//

import Foundation

struct Bytecode {
    init(_ instructions: Instructions, _ constants: [JpfObject]) {self.instructions = instructions; self.constants = constants}
    let instructions: Instructions
    let constants: [JpfObject]
}
//
class Compiler {
    init(from node: Node) {self.node = node}
    private let node: Node
    private var instructions: Instructions = []
    private var constants: [JpfObject] = []
    private var stack: [JpfObject] = []
    var bytecode: Bytecode {
        Bytecode(instructions, constants)
    }
    //
    /// 指定されたASTノードをコンパイルする。
    /// - Returns: エラー(無しは、nil)
    func compile() -> JpfError? {
        if let error = node.compiled(with: self) as? JpfError {return error}
        return nil
    }
    /// インストラクションを出力し、新たなインストラクション位置を返す。
    /// - Parameters:
    ///   - op: オペコード
    ///   - operands: オペランド
    /// - Returns: 新たなip(インストラクション・ポイント)
    func emit(op: Opcode, operands: [Int] = []) -> Int {
        let instruction = make(op: op, operands: operands)
        return addInstruction(instruction)
    }
    func emit(op: Opcode, operand: Int) -> Int {
        return emit(op: op, operands: [operand])
    }
    /// インストラクションを記録(追加)し、新たなインストラクション位置を返す。
    /// - Parameter ins: 追加するバイト列
    /// - Returns: 新たなip(インストラクション・ポイント)
    private func addInstruction(_ ins: [Byte]) -> Int {
        let postionOfNewInstruction = instructions.count
        instructions += ins
        return postionOfNewInstruction
    }
    /// 定数(JpfObject)を記録(追加)し、追加位置を返す。
    /// - Parameter obj: 追加するオブジェクト
    /// - Returns: 追加位置
    func addConstant(_ obj: JpfObject) -> Int {
        constants.append(obj)
        return constants.count - 1
    }
    // 定数の演算を行うための補助(ヘルパー)
    func push(_ object: JpfObject)  {stack.append(object)}
    func pull() -> JpfObject?       {stack.popLast()}
    var peek: JpfObject?            {stack.last}
    func drop()                     {_ = pull()}
    func drop(_ n: Int)             {stack.removeLast(n <= count ? n : count)}
    var count: Int                  {stack.count}
    var isEmpty: Bool               {stack.isEmpty}
    func unwrappedObject() -> JpfObject? {
        return (pull() as? JpfPhrase)?.value
    }
    var unwrappedPeek: JpfObject? {
        return (peek as? JpfPhrase)?.value ?? peek
    }
    func peek(_ n: Int) -> [JpfObject]? {
        guard n <= count else {return nil}
        return Array(stack[(count - n)..<count])
    }
}
