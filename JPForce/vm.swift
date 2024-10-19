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
        //
        frames = [Frame](repeating: Frame(), count: maxFrames)
        let mainFunction = JpfCompiledFunction(instructions: bytecode.instructions)
        let mainFrame = Frame(with: mainFunction, basePointer: 0)
        frames[framesIndex] = mainFrame
        framesIndex += 1
    }
    convenience init(with bytecode: Bytecode, _ globals: GlobalStore, _ stack: Stack) {
        self.init(with: bytecode)
        self.globals = globals
        self.stack = stack
    }
    //
    private let constants: [JpfObject]
    var stack: Stack
    private var globals: GlobalStore
    //
    private let maxFrames = 1024
    private var frames: [Frame] = []
    private var framesIndex: Int = 0
    // エラー
    private let codeNotSupported =          JpfError("は、未実装。")
    private let failedToSetGlobal =         JpfError("大域変数の定義に失敗した。")
    private let failedToBuildArray =        JpfError("配列の定義に失敗した。")
    private let failedToBuildDictionary =   JpfError("辞書の定義に失敗した。")
    // Fetch instructions
    func run() -> JpfError? {
        while currentFrame.isIpInRange {
            let ip = currentFrame.advancedIp()
            let instructions = currentFrame.insturctions!
            let opcode = Opcode(rawValue: instructions[ip])!
            switch opcode {
            case .opConstant:
                let constIndex = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                currentFrame.advanceIp(by: 2)
                if let error = push(constants[constIndex]) {return error}
            case .opSetGlobal:
                let globalIndex = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                currentFrame.advanceIp(by: 2)
                guard let result = pull() else {return failedToSetGlobal}
                globals[globalIndex] = result
            case .opGetGlobal:
                let globalIndex = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                currentFrame.advanceIp(by: 2)
                if let error = push(globals[globalIndex]) {return error}
            case .opSetLocal:
                let localIndex = Int(readUInt8(from: Array(instructions[(ip+1)...])))
                currentFrame.advanceIp(by: 1)
                stack[currentFrame.basePointer + localIndex] = pull()
            case .opGetLocal:
                let localIndex = Int(readUInt8(from: Array(instructions[(ip+1)...])))
                currentFrame.advanceIp(by: 1)
                if let error = push(stack[currentFrame.basePointer + localIndex]!) {return error}
            case .opArray:
                let numberOfElements = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                currentFrame.advanceIp(by: 2)
                guard let array = buildArray(with: numberOfElements) else {return failedToBuildArray}
                if let error = push(array) {return error}
            case .opDictionary:
                let numberOfElements = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                currentFrame.advanceIp(by: 2)
                guard let dictionary = buildDictionary(with: numberOfElements) else {return failedToBuildDictionary}
                if let error = push(dictionary) {return error}
            case .opPhrase: // スタックのオブジェクトと、定数からJpfPhraseオブジェクトを作成
                let constIndex = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                currentFrame.advanceIp(by: 2)
                let p = constants[constIndex] as! JpfPhrase
                let phrase = JpfPhrase(value: pull()?.value, particle: p.particle)
                if let error = push(phrase) {return error}
            case .opPop:
                _ = pull()
            case .opJump:
                let position = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                currentFrame.setIp(to: position - 1)    // 飛び先
            case .opJumpNotTruthy:
                let position = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                currentFrame.advanceIp(by: 2)
                if let condition = pull(), !condition.isTrue {  // Not Truthy
                    currentFrame.setIp(to: position - 1)    // 飛び先
                }
            default:
                guard let executer = CodeExecutableFactory.create(from: opcode, with: self) else {return "命令語(\(definitions[opcode]!.name))" + codeNotSupported}
                if let error = executer.execute() {return error}
            }
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
    //
    var currentFrame: Frame {frames[framesIndex - 1]}
    func push(_ frame: Frame) {
        frames[framesIndex] = frame
        framesIndex += 1
    }
    func popFrame() -> Frame {
        framesIndex -= 1
        return frames[framesIndex]
    }
    // Stack accessor
    var stackTop: JpfObject?                        {stack.top}
    var sp: Int                                     {
        get {stack.stackPointer}
        set {stack.stackPointer = newValue}
    }
    var string: String                              {stack.string}
    func push(_ object: JpfObject) -> JpfError?     {stack.push(object)}
    func push(_ objects: [JpfObject]) -> JpfError?  {stack.push(objects)}
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
