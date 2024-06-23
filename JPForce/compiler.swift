//
//  compiler.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/06/19.
//

import Foundation

class Compiler {
    let node: Node
    init(from node: Node) {self.node = node}
    var instructions: Instructions = []
    var constants: [JpfObject] = []
    var bytecode: Bytecode {
        Bytecode(instructions, constants)
    }
    //
    /// 指定されたASTノードをコンパイルする。
    /// - Returns: エラー(無しは、nil)
    func compile() -> JpfError? {
        node.compiled(with: self)
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
    func addInstruction(_ ins: [Byte]) -> Int {
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
}
struct Bytecode {
    let instructions: Instructions
    let constants: [JpfObject]
    init(_ instructions: Instructions, _ constants: [JpfObject]) {self.instructions = instructions; self.constants = constants}
}
