//
//  compiler.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/06/19.
//

import Foundation

/// バイトコード(インストラクションと定数表)
struct Bytecode {
    init(_ instructions: Instructions, _ constants: [JpfObject]) {self.instructions = instructions; self.constants = constants}
    let instructions: Instructions
    let constants: [JpfObject]
}
/// 出力インストラクション
struct EmittedInstruction {
    let opcode: Opcode
    let position: Int
}
/// スコープ単位のインストラクション
class CompilationScope {
    var instructions: Instructions = []             // 現インストラクション列
    var lastInstruction: EmittedInstruction?        // 直前のインストラクション
    var previousInstruction: EmittedInstruction?    // 前インストラクション
    //
    func append(_ instruction: Instruction) {       // インストラクションを追加
        self.instructions.bytes += instruction
    }
    subscript(index: Int) -> Byte {                 // インストラクション中のコードアクセス
        get {instructions[index]}
        set {instructions[index] = newValue}
    }
    func setLastInstruction(op: Opcode, at pos: Int) {  // 最新のインストラクションを設定
        previousInstruction = lastInstruction
        lastInstruction = EmittedInstruction(opcode: op, position: pos)
    }
}
/// 翻訳器
class Compiler {
    init(from node: Node) {self.node = node}
    convenience init(from node: Node, _ symbolTable: SymbolTable, _ constants: [JpfObject]) {
        self.init(from: node)
        self.symbolTable = symbolTable
        self.constants = constants
    }
    private let node: Node          // コンパイルするASTノード
    private var constants: [JpfObject] = [] // 定数表
    var symbolTable = SymbolTable() // シンボルテーブル
    let environment = Environment() // 定数計算を行うstackを提供する。
    var scopes: [CompilationScope] = [CompilationScope()]   // main scope
    var scopeIndex = 0
    //
    var bytecode: Bytecode {Bytecode(currentInstructions, constants)}   // バイトコードを返す
    var lastPosition: Int {currentInstructions.count}                   // 最新インストラクション・ポイント
    var currentScope: CompilationScope {scopes[scopeIndex]}             // 現スコープ
    var currentInstructions: Instructions {                             // 現スコープのインストラクション列
        get {currentScope.instructions}
        set {currentScope.instructions = newValue}
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
        let position = addInstruction(instruction)
        currentScope.setLastInstruction(op: op, at: position)
        return position
    }
    func emit(op: Opcode, operand: Int) -> Int {
        return emit(op: op, operands: [operand])
    }
    /// インストラクションを記録(追加)し、新たなインストラクション位置を返す。
    /// - Parameter ins: 追加するバイト列
    /// - Returns: 新たなip(インストラクション・ポイント)
    private func addInstruction(_ instruction: Instruction) -> Int {
        let postionOfNewInstruction = lastPosition
        currentScope.append(instruction)
        return postionOfNewInstruction
    }
    /// 定数(JpfObject)を記録(追加)し、追加位置を返す。
    /// - Parameter obj: 追加するオブジェクト
    /// - Returns: 追加位置
    func addConstant(_ obj: JpfObject) -> Int {
        constants.append(obj)
        return constants.count - 1
    }
    var lastOpcode: Opcode? {
        return currentScope.lastInstruction?.opcode
    }
    /// 直前の(定数表の)句の助詞をチェック
    /// - Parameter particle: 助詞
    /// - Returns: 助詞が一致
    func isLastPhrase(particle: Token.Particle) -> Bool {
        if lastOpcode == .opPhrase, let phrase = constants.last as? JpfPhrase {
            return phrase.isParticle(particle)
        }
        return false
    }
    /// 指定位置のオペランドを書き換える。
    /// - Parameters:
    ///   - opPosition: オペコードの位置
    ///   - operand: 書き換えるオペランド
    func changeOperand(at opPosition: Int, operand: Int) {
        let op = Opcode(rawValue: currentScope[opPosition])!
        let instruction = make(op: op, operand: operand)
        replaceInstruction(at: opPosition, newInstruction: instruction)
    }
    func changeConstant(at pos: Int, with constant: JpfObject) {
        constants[pos] = constant
    }
    func changeLastConstant(with constant: JpfObject) {
        guard !constants.isEmpty else {return}
        constants[constants.count - 1] = constant
    }
    func setLastConstant(name: String) {
        guard !constants.isEmpty else {return}
        constants[constants.count - 1].name = name
    }
    func removeLastInstruction() {
        guard let lastEmittied = currentScope.lastInstruction else {return}
        let previousEmitted = currentScope.previousInstruction
        let old = currentInstructions
        let new = Instructions(old.bytes.dropLast(1 + lastEmittied.opcode.operandWidth))
        currentInstructions = new
        currentScope.lastInstruction = previousEmitted
        if lastEmittied.opcode == .opConstant || lastEmittied.opcode == .opPhrase {
            constants.removeLast()
        }
    }
    /// 句の格が正しければ、その句を出力から取り除く
    /// - Parameter particle: 格
    /// - Returns: 格の成否
    func removeLastPhrase(particle: Token.Particle) -> Bool {
        guard isLastPhrase(particle: particle) else {return false}
        removeLastInstruction()
        return true
    }
    /// 指定位置のインストラクションを置き換える。
    /// - Parameters:
    ///   - pos: 置き換える位置
    ///   - newInstructions: 置き換えるインストラクション
    private func replaceInstruction(at pos: Int, newInstruction: Instruction) {
        newInstruction.enumerated().forEach { index, byte in
            currentScope[pos + index] = byte
        }
    }
    /// Scope制御
    func enterScope() {
        scopes.append(CompilationScope())
        scopeIndex += 1
        symbolTable = SymbolTable(outer: symbolTable)
    }
    func leaveScope() -> Instructions {
        let instructions = currentInstructions
        scopes.removeLast()
        scopeIndex -= 1
        symbolTable = symbolTable.outer!
        return instructions
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
