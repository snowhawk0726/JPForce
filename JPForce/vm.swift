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
    init() {
        self.stack = [JpfObject](repeating: JpfNull.object, count: stackSize)
        self.sp = 0
    }
    private let stackSize = 2048
    private var stack: [JpfObject]
    private var sp: Int // 次の値を示す。スタックトップは、stack[sp-1]
    //
    private let stackOverflow = JpfError("スタックが上限を超えた(overflow)。")
    //
    var isEmpty: Bool {sp <= 0}
    var count: Int {sp}
    var top: JpfObject? {peek()}
    var string: String {stack.prefix(sp).map {$0.string}.joined(separator: " ")}
    subscript(index: Int) -> JpfObject? {
        get {
            guard case 0..<sp = index else {return nil}
            return stack[index]
        }
        set {stack[index] = newValue!}
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
    /// (オブジェクトは、スタックの末尾から探す)
    func pull(where condition: (JpfObject) -> Bool) -> JpfObject? {
        guard let index = (0..<sp).last(where: {condition(stack[$0])}) else {
            return nil
        }
        let object = stack[index]
        if index != sp - 1 {
            stack[index] = stack[sp - 1]            // 最後の要素と置き換え
        }
        sp -= 1                                     // スタックポインタをデクリメント
        return object
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
    var stackPointer: Int {
        get {return sp}
        set {sp = newValue}
    }
    /// 指定された要素数を、destinationに積み替える。
    /// (スタックポインタは積み替え先に移動)
    /// - Parameters:
    ///   - number: pushされた要素数
    ///   - destination: 積み替え先
    func move(numberOfPushed: Int, to destination: Int) {
        let elements = peek(numberOfPushed)!
        sp = destination
        _ = push(elements)
    }
}

class VM {
    init(with bytecode: Bytecode) {
        self.constants = bytecode.constants
        self.globals = GlobalStore()
        self.stack = Stack()
        // メインフレームを含むフレームを作成
        frames = [Frame](repeating: Frame(), count: maxFrames)
        let mainFunction = JpfCompiledFunction(instructions: bytecode.instructions)
        let mainClosure = JpfClosure(with: mainFunction)
        let mainFrame = Frame(with: mainClosure, basePointer: 0)
        frames[framesIndex] = mainFrame
        framesIndex += 1
    }
    convenience init(with bytecode: Bytecode, _ globals: GlobalStore, _ stack: Stack) {
        self.init(with: bytecode)
        self.globals = globals
        self.stack = stack
    }
    //
    let constants: [JpfObject]  // 定数
    var stack: Stack            // スタック
    var globals: GlobalStore    // 大域領域
    // フレーム
    private let maxFrames = 1024
    private var frames: [Frame] = []
    private var framesIndex: Int = 0
    // エラー
    private let invalidOpcode = JpfError("不正なオペコード:")
    /// Fetch instructions
    func run() -> JpfError? {
        while currentFrame.isIpInRange {
            var ip = currentFrame.advanceIp()
            let instructions = currentFrame.insturctions!
            let op = instructions[ip]
            guard let opcode = Opcode(rawValue: op) else {return invalidOpcode + "\(op)"}
            ip += 1
            let bytes = instructions[ip..<ip+opcode.operandWidth].bytes
            let executer = CodeExecutableFactory.create(from: opcode, operandBytes: bytes, with: self)
            do {
                try executer.execute()
            } catch {
                return jpfError(from: error)
            }
        }
        return nil
    }
    //
    var currentFrame: Frame {frames[framesIndex - 1]}
    func push(_ frame: Frame) {
        frames[framesIndex] = frame
        framesIndex += 1
    }
    func popFrame() -> Frame? {
        guard framesIndex > 1 else {return nil}
        framesIndex -= 1
        return frames[framesIndex]
    }
    // Stack accessor
    var stackTop: JpfObject?                        {stack.top}
    var sp: Int                                     {
        get {stack.stackPointer}
        set {stack.stackPointer = newValue}
    }
    var count: Int                                  {stack.count}
    var string: String                              {stack.string}
    func push(_ object: JpfObject) throws           {
        if let err = stack.push(object) {throw err}
    }
    func push(_ objects: [JpfObject]) throws        {
        if let err = stack.push(objects) {throw err}
    }
    func pull() -> JpfObject?                       {stack.pull()}
    func pullAll() -> [JpfObject]                   {stack.pullAll()}
    func peek() -> JpfObject?                       {stack.peek()}
    func peek(_ n: Int) -> [JpfObject]?             {stack.peek(n)}
    func peekAll() -> [JpfObject]                   {stack.getAll()}
    func drop()                                     {stack.drop()}
    func drop(_ n: Int)                             {stack.drop(n)}
    /// currentから、スタックに積まれた要素をdestinationに移す。
    /// - Parameters:
    ///   - current: スタックに積まれる前の位置
    ///   - destination: 移動先
    func movePushed(from current: Int, to destination: Int) {
        let numberOfElements = sp - current         // pushされた要素数
        stack.move(numberOfPushed: numberOfElements, to: destination)
    }
}
