//
//  vm.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/06/21.
//

import Foundation

class GlobalStore {
    private let globalsSize = 65536
    private var globals: [JpfObject]
    init() {
        self.globals = [JpfObject](repeating: JpfNull.object, count: globalsSize)
    }
    subscript(index: Int) -> JpfObject {
        get {globals[index]}
        set {globals[index] = newValue}
    }
}

class Stack {
    private let stackSize = 2048
    private var stack: [JpfObject]
    private var sp: Int // 次の値を示す。スタックトップは、stack[sp-1]
    init() {
        self.stack = [JpfObject](repeating: JpfNull.object, count: stackSize)
        self.sp = 0
    }
    //
    var isEmpty: Bool {sp <= 0}
    var string: String {stack.prefix(sp).map {$0.string}.joined(separator: " ")}
    var top: JpfObject? {peek()}
    //
    func push(_ object: JpfObject) -> JpfError? {
        guard sp < stackSize else {
            return JpfError("stack overflow")
        }
        stack[sp] = object
        sp += 1
        return nil
    }
    func pull() -> JpfObject {
        if isEmpty {
            return JpfError("stack underflow")
        }
        let object = stack[sp - 1]
        sp -= 1
        return object
    }
    func peek() -> JpfObject? {
        if isEmpty {return nil}
        return stack[sp - 1]
    }
    func peek(_ n: Int) -> [JpfObject]? {
        guard n > 0 && n <= sp else { return nil }
        return Array(stack[(sp - n)..<sp])
    }
    func drop() {
        if isEmpty {return}
        sp -= 1
    }
    func drop(_ n: Int) {
        if isEmpty {return}
        sp -= n
    }
}

class VM {
    private let constants: [JpfObject]
    private let instructions: Instructions
    private var stack: Stack
    private var globals: GlobalStore
    init(with bytecode: Bytecode) {
        self.instructions = bytecode.instructions
        self.constants = bytecode.constants
        self.globals = GlobalStore()
        self.stack = Stack()
    }
    convenience init(with bytecode: Bytecode, _ globals: GlobalStore, _ stack: Stack) {
        self.init(with: bytecode)
        self.globals = globals
        self.stack = stack
    }
    // Fetch instructions
    func run() -> JpfError? {
        var ip = 0  // instructionsの位置を示す。
        while ip < instructions.count {
            let opcode = Opcode(rawValue: instructions[ip])!
            switch opcode {
            case .opConstant:
                let constIndex = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                ip += 2
                if let error = push(constants[constIndex]) {return error}
            case .opSetGlobal:
                let globalIndex = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                ip += 2
                globals[globalIndex] = pull()
            case .opGetGlobal:
                let globalIndex = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                ip += 2
                if let error = push(globals[globalIndex]) {return error}
            case .opPop:
                _ = pull()
            case .opJump:
                let position = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                ip = position - 1           // 飛び先
            case .opJumpNotTruthy:
                let position = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                ip += 2
                let condition = pull()
                if !condition.isTrue {      // Not Truthy
                    ip = position - 1       // 飛び先
                }
            default:
                guard let executer = CodeExecutableFactory.create(from: opcode, with: self) else {return codeNotSupported + "(命令語：\(definitions[opcode]!.name))"}
                if let error = executer.execute() {return error}
            }
            ip += 1
        }
        return nil
    }
    // Stack accessor
    var stackTop: JpfObject? {stack.top}
    var string: String {stack.string}
    func push(_ object: JpfObject) -> JpfError? {stack.push(object)}
    func pull() -> JpfObject {stack.pull()}
    func peek() -> JpfObject? {stack.peek()}
    func peek(_ n: Int) -> [JpfObject]? {stack.peek(n)}
    func drop() {stack.drop()}
    func drop(_ n: Int) {stack.drop(n)}
    // エラー
    let codeNotSupported = JpfError("該当する命令語は、未実装。")
}
