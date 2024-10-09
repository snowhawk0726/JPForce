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
    private let stackOverflow = JpfError("スタックが上限を超えた(overflow)。")
    //
    var isEmpty: Bool {sp <= 0}
    var count: Int {sp}
    var top: JpfObject? {peek()}
    var string: String {stack.prefix(sp).map {$0.string}.joined(separator: " ")}
    subscript(index: Int) -> JpfObject? {
        guard case 0..<sp = index else {return nil}
        return stack[index]
    }
    //
    func push(_ object: JpfObject) -> JpfError? {
        guard sp < stackSize else {
            return stackOverflow
        }
        stack[sp] = object
        sp += 1
        return nil
    }
    func push(_ objects: [JpfObject]) -> JpfError? {
        guard sp + objects.count <= stackSize else {
            return stackOverflow
        }
        stack.replaceSubrange(sp..<sp + objects.count, with: objects)
        sp += objects.count
        return nil
    }
    func pull() -> JpfObject? {
        if isEmpty {return nil}
        let object = stack[sp - 1]
        sp -= 1
        return object
    }
    /// スタックに指定のオブジェクトがあったら、それを取り出す。(無ければ nil)
    func pull(where condition: (JpfObject) -> Bool) -> JpfObject? {
        for i in stride(from: sp - 1, through: 0, by: -1) {  // スタックの末尾から検索
            if condition(stack[i]) {
                let object = stack[i]
                stack[i] = stack[sp - 1]  // 最後の要素と置き換え
                sp -= 1  // スタックポインタをデクリメント
                return object
            }
        }
        return nil
    }
    func pullAll() -> [JpfObject] {
        defer {sp = 0}
        return getAll()
    }
    func getAll() -> [JpfObject] {
        guard !isEmpty else {return []}
        return Array(stack[0..<sp])
    }
    func peek() -> JpfObject? {
        if isEmpty {return nil}
        return stack[sp - 1]
    }
    func peek(_ n: Int) -> [JpfObject]? {
        guard n >= 0 && n <= sp else {return nil}   // n = 0の場合、[]が返る
        return Array(stack[(sp - n)..<sp])
    }
    func drop() {
        if isEmpty {return}
        sp -= 1
    }
    func drop(_ n: Int) {
        sp -= n < count ? n : count
    }
    func empty() {sp = 0}
    func swap() {if sp >= 2 {stack.swapAt(sp - 2, sp - 1)}}
}

class VM {
    private let constants: [JpfObject]
    private let instructions: Instructions
    var stack: Stack
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
    // エラー
    private let codeNotSupported =          JpfError("は、未実装。")
    private let failedToSetGlobal =         JpfError("大域変数の定義に失敗した。")
    private let failedToBuildArray =        JpfError("配列の定義に失敗した。")
    private let failedToBuildDictionary =   JpfError("辞書の定義に失敗した。")
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
                guard let result = pull() else {return failedToSetGlobal}
                globals[globalIndex] = result
            case .opGetGlobal:
                let globalIndex = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                ip += 2
                if let error = push(globals[globalIndex]) {return error}
            case .opArray:
                let numberOfElements = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                ip += 2
                guard let array = buildArray(with: numberOfElements) else {return failedToBuildArray}
                if let error = push(array) {return error}
            case .opDictionary:
                let numberOfElements = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                ip += 2
                guard let dictionary = buildDictionary(with: numberOfElements) else {return failedToBuildDictionary}
                if let error = push(dictionary) {return error}
            case .opPop:
                _ = pull()
            case .opJump:
                let position = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                ip = position - 1           // 飛び先
            case .opJumpNotTruthy:
                let position = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                ip += 2
                if let condition = pull(), !condition.isTrue {  // Not Truthy
                    ip = position - 1       // 飛び先
                }
            default:
                guard let executer = CodeExecutableFactory.create(from: opcode, with: self) else {return "命令語(\(definitions[opcode]!.name))" + codeNotSupported}
                if let error = executer.execute() {return error}
            }
            ip += 1
        }
        return nil
    }
    private func buildArray(with n: Int) -> JpfArray? {
        guard let objects = peek(n) else {return nil}
        drop(n)
        return JpfArray(elements: objects)
    }
    private func buildDictionary(with n: Int) -> JpfDictionary? {
        var pairs: [JpfHashKey: (key: JpfObject, value: JpfObject)] = [:]
        guard let objects = peek(n) else {return nil}
        drop(n)
        for i in stride(from: 0, to: n, by: 2) {
            let key = objects[i], value = objects[i+1]
            guard let hash = key as? JpfHashable else {return nil}
            pairs[hash.hashKey] = (key, value)
        }
        return JpfDictionary(pairs: pairs)
    }
    // Stack accessor
    var stackTop: JpfObject?                        {stack.top}
    var string: String                              {stack.string}
    func push(_ object: JpfObject) -> JpfError?     {stack.push(object)}
    func push(_ objects: [JpfObject]) -> JpfError?  {stack.push(objects)}
    func pull() -> JpfObject?                       {stack.pull()}
    func pullAll() -> [JpfObject]                   {stack.pullAll()}
    func peek() -> JpfObject?                       {stack.peek()}
    func peek(_ n: Int) -> [JpfObject]?             {stack.peek(n)}
    func drop()                                     {stack.drop()}
    func drop(_ n: Int)                             {stack.drop(n)}
}
