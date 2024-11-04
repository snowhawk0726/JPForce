//
//  codeExecutable.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/09/08.
//

import Foundation

// MARK: - intefaces for execution
protocol CodeExecutable {
    var vm: VM {get}
    /// バイトコードを実行する。
    /// - Returns: 正常時はnil
    func execute() -> JpfError?
}
// MARK: - implementations for CodeExecutable
extension CodeExecutable {
    // エラー
    var genitiveParamError: JpfError        {JpfError("属格の左右オブジェクトが足りない。")}
    var callingFunctionNotFound: JpfError   {JpfError("呼び出し先の関数が無い。")}
    var returnValueNotFound: JpfError       {JpfError("返すべき値が無い。")}
    var failedToSetGlobal: JpfError         {JpfError("大域変数の定義に失敗した。")}
    var failedToBuildArray: JpfError        {JpfError("配列の定義に失敗した。")}
    var failedToBuildDictionary: JpfError   {JpfError("辞書の定義に失敗した。")}
    var failedToGetProperty: JpfError       {JpfError("属性の取得に失敗した。")}
    var cannotFoundPredicate: JpfError      {JpfError("述語が定義されていない。")}
}
// MARK: - instance factory
struct CodeExecutableFactory {
    static func create(from op: Opcode, operandBytes: [Byte], with vm: VM) -> CodeExecutable? {
        switch op {
        case .opTrue,.opFalse:  return BooleanExecuter(vm, by: op)
        case .opNull:           return NullExecuter(vm)
        case .opArray:          return ArrayExecuter(vm, with: operandBytes)
        case .opDictionary:     return DictionaryExecuter(vm, with: operandBytes)
        case .opGenitive:       return GenitiveExecuter(vm)
        case .opJump, .opJumpNotTruthy:
                                return JumpExecuter(vm, by: op, with: operandBytes)
        case .opCall:           return CallExecuter(vm)
        case .opReturnValue:    return ReturnValueExecuter(vm)
        case .opReturn:         return ReturnExecuter(vm)
        case .opConstant:       return ConstantExecuter(vm, with: operandBytes)
        case .opPhrase:         return PhraseExecuter(vm, with: operandBytes)
        case .opPop:            return PopExecuter(vm)
        case .opSetGlobal:      return SetGlobalExecuter(vm, with: operandBytes)
        case .opGetGlobal:      return GetGlobalExecuter(vm, with: operandBytes)
        case .opSetLocal:       return SetLocalExecuter(vm, with: operandBytes)
        case .opGetLocal:       return GetLocalExecuter(vm, with: operandBytes)
        case .opGetProperty:    return GetPropertyExecuter(vm, with: operandBytes)
        case .opPredicate:      return PredicateExecuter(vm, with: operandBytes)
        }
    }
}
// MARK: - implementation for executers
struct BooleanExecuter : CodeExecutable {
    init(_ vm: VM, by op: Opcode) {self.vm = vm; self.op = op}
    let vm: VM, op: Opcode
    func execute() -> JpfError? {
        if let error = vm.push(JpfBoolean.object(of: op == .opTrue)) {return error}
        return nil
    }
}
struct NullExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        if let error = vm.push(JpfNull.object) {return error}
        return nil
    }
}
struct ArrayExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() -> JpfError? {
        let numberOfElements = Int(readUInt16(from: bytes))
        vm.currentFrame.advanceIp(by: 2)
        guard let array = buildArray(with: numberOfElements) else {return failedToBuildArray}
        if let error = vm.push(array) {return error}
        return nil
    }
    private func buildArray(with n: Int) -> JpfArray? {
        guard let objects = vm.peek(n) else {return nil}
        vm.drop(n)
        return JpfArray(elements: objects)
    }
}
struct DictionaryExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() -> JpfError? {
        let numberOfElements = Int(readUInt16(from: bytes))
        vm.currentFrame.advanceIp(by: 2)
        guard let dictionary = buildDictionary(with: numberOfElements) else {return failedToBuildDictionary}
        if let error = vm.push(dictionary) {return error}
        return nil
    }
    private func buildDictionary(with n: Int) -> JpfDictionary? {
        var pairs: [JpfHashKey: (key: JpfObject, value: JpfObject)] = [:]
        guard let objects = vm.peek(n) else {return nil}
        vm.drop(n)
        for i in stride(from: 0, to: n, by: 2) {
            let key = objects[i], value = objects[i+1]
            guard let hash = key as? JpfHashable else {return nil}
            pairs[hash.hashKey] = (key, value)
        }
        return JpfDictionary(pairs: pairs)
    }
}
struct GenitiveExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        guard let index = vm.pull(), let left = vm.pull() else {return genitiveParamError}
        let environment = Environment(with: vm.stack)
        guard let result = left.accessed(by: index, with: environment) else {return nil}
        if result.isError {return result.error}
        if let error = vm.push(result) {return error}
        return nil
    }
}
struct CallExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        guard let function = vm.pull()?.value as? JpfCompiledFunction else {return callingFunctionNotFound}
        let arguments = vm.peekAll()                        // 引数候補
        // TODO: - 引数をチェックし、実行する関数を特定し、Frameに割り当てる。
        guard arguments.count >= function.numberOfParameters else {
            return InputFormatError.numberOfParameters(function.numberOfParameters).message
        }
        vm.drop(function.numberOfParameters)
        let frame = Frame(with: function, basePointer: vm.sp)
        vm.push(frame)
        // TODO: - 助詞をチェックし、引数をローカル変数に割り当てる。
        if let error = vm.push(Array(arguments.suffix(function.numberOfParameters)).map {$0.value!}) {return error}
        vm.sp = frame.basePointer + function.numberOfLocals // ローカル変数のスロットを確保
        return nil
    }
}
struct ReturnValueExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        guard vm.sp > vm.currentFrame.sp,                   // spがローカル変数スロット外
              let returnValue = vm.pull() else {return returnValueNotFound}
        if let frame = vm.popFrame() {
            vm.movePushed(from: frame.sp, to: frame.basePointer)
        }
        if let error = vm.push(returnValue) {return error}
        return nil
    }
}
struct ReturnExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        if let frame = vm.popFrame() {
            vm.movePushed(from: frame.sp, to: frame.basePointer)
        }
        return nil
    }
}
struct JumpExecuter : CodeExecutable {
    init(_ vm: VM, by op: Opcode, with bytes: [Byte]) {self.vm = vm; self.op = op; self.bytes = bytes}
    let vm: VM, op: Opcode, bytes: [Byte]
    func execute() -> JpfError? {
        let position = Int(readUInt16(from: bytes))
        vm.currentFrame.advanceIp(by: 2)
        if op == .opJump || isNotTruthy {
            vm.currentFrame.setIp(to: position - 1) // 飛び先にjump
        }
        return nil
    }
    var isNotTruthy: Bool {
        if let condition = vm.pull() {return !condition.isTrue}
        return false
    }
}
struct ConstantExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() -> JpfError? {
        let constIndex = Int(readUInt16(from: bytes))
        vm.currentFrame.advanceIp(by: 2)
        if let error = vm.push(vm.constants[constIndex]) {return error}
        return nil
    }
}
struct PhraseExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() -> JpfError? {
        let constIndex = Int(readUInt16(from: bytes))
        vm.currentFrame.advanceIp(by: 2)
        let p = vm.constants[constIndex] as! JpfPhrase
        if let value = vm.pull() {
            let phrase = JpfPhrase(value: value, particle: p.particle)
            if let error = vm.push(phrase) {return error}
        }
        return nil
    }
}
struct PopExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        vm.drop()
        return nil
    }
}
struct SetGlobalExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() -> JpfError? {
        let globalIndex = Int(readUInt16(from: bytes))
        vm.currentFrame.advanceIp(by: 2)
        guard let result = vm.pull() else {return failedToSetGlobal}
        vm.globals[globalIndex] = result
        return nil
    }
}
struct GetGlobalExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() -> JpfError? {
        let globalIndex = Int(readUInt16(from: bytes))
        vm.currentFrame.advanceIp(by: 2)
        if let error = vm.push(vm.globals[globalIndex]) {return error}
        return nil
    }
}
struct SetLocalExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() -> JpfError? {
        let localIndex = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        vm.stack[vm.currentFrame.basePointer + localIndex] = vm.pull()
        return nil
    }
}
struct GetLocalExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() -> JpfError? {
        let localIndex = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        if let error = vm.push(vm.stack[vm.currentFrame.basePointer + localIndex]!) {return error}
        return nil
    }
}
struct GetPropertyExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() -> JpfError? {
        let index = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        guard let getPropertyOf = ObjectProperties()[index]?.accessor,
              let object = vm.pull(),
              let result = getPropertyOf(object) else {
            return failedToGetProperty
        }
        if result.isError {return result.error}
        if let error = vm.push(result) {return error}
        return nil
    }
}
struct PredicateExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() -> JpfError? {
        let index = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        guard let predicate = PredicateOperableFactory()[index] else {return cannotFoundPredicate}
        let environment = Environment(with: vm.stack)
        guard let result = predicate(environment).operated() else {return nil}
        if result.isError {return result.error}
        if let error = vm.push(result) {return error}
        return nil
    }
}
