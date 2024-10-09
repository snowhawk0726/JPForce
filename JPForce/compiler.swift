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
    convenience init(from node: Node, _ symbolTable: SymbolTable, _ constants: [JpfObject]) {
        self.init(from: node)
        self.symbolTable = symbolTable
        self.constants = constants
    }
    private let node: Node
    private var constants: [JpfObject] = []
    var symbolTable = SymbolTable() // シンボルテーブル
    let environment = Environment() // 定数計算を行うstackを提供する。
    var instructions: Instructions = []
    //
    var bytecode: Bytecode {
        Bytecode(instructions, constants)
    }
    var lastPosition: Int {instructions.count}  // 最新インストラクション・ポイント
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
        let position = addInstruction(instruction)
        return position
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
    private func replaceInstruction(at pos: Int, newInstructions: Instructions) {
        for i in 0..<newInstructions.count {
            instructions[pos + i] = newInstructions[i]
        }
    }
    func changeOperand(at opPosition: Int, operand: Int) {
        let op = Opcode(rawValue: instructions[opPosition])!
        let instruction = make(op: op, operand: operand)
        replaceInstruction(at: opPosition, newInstructions: instruction)
    }
    // 定数の演算を行うための補助(ヘルパー)
    func push(_ o: JpfObject) -> JpfError?
                                    {environment.push(o)}
    func pull() -> JpfObject?       {environment.pull()}
    func pullAll() -> [JpfObject]   {environment.pullAll()}
    var peek: JpfObject?            {environment.peek}
    func peek(_ n: Int) -> [JpfObject]? {environment.peek(n)}
    func drop()                     {environment.drop()}
    func drop(_ n: Int)             {environment.drop(n)}
    var count: Int                  {environment.count}
    var isEmpty: Bool               {environment.isEmpty}
    func unwrappedObject() -> JpfObject?
                                    {(pull() as? JpfPhrase)?.value}
    var unwrappedPeek: JpfObject?   {(peek as? JpfPhrase)?.value ?? peek}
}
