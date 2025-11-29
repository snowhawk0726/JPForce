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
    var propertyNotFound: JpfError          {JpfError("指定した属性が見つからない。")}
    var objectNotFound: JpfError            {JpfError("対象のオブジェクトが見つからない。")}
    var failedToGetProperty: JpfError       {JpfError("属性の取得に失敗した。")}
    var failedToGetFunction: JpfError       {JpfError("翻訳済み関数の取得に失敗した。")}
    var failedToGetClosure: JpfError        {JpfError("クロージャの取得に失敗した。")}
    var failedToGetFreeVariables: JpfError  {JpfError("自由変数の取得に失敗した。")}
    var cannotFoundPredicate: JpfError      {JpfError("述語が定義されていない。")}
}
// MARK: - instance factory
struct CodeExecutableFactory {
    static func create(from op: Opcode, operandBytes: [Byte], with vm: VM) -> CodeExecutable {
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
        case .opClosure:        return ClosureExecuter(vm, with: operandBytes)
        case .opGetFree:        return GetFreeExecuter(vm, with: operandBytes)
        case .opCurrentClosure: return CurrentClosureExecuter(vm)
        }
    }
}
// MARK: - implementation for executers
struct BooleanExecuter : CodeExecutable {
    init(_ vm: VM, by op: Opcode) {self.vm = vm; self.op = op}
    let vm: VM, op: Opcode
    func execute() -> JpfError? {
        vm.push(JpfBoolean.object(of: op == .opTrue))
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
        return vm.push(array)
    }
}
private extension ArrayExecuter {
    func buildArray(with n: Int) -> JpfArray? {
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
        return vm.push(dictionary)
    }
}
private extension DictionaryExecuter {
    func buildDictionary(with n: Int) -> JpfDictionary? {
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
        return vm.push(result)
    }
}
struct CallExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        guard let closure = vm.pull()?.value as? JpfClosure,
              let function = closure.fn else {return callingFunctionNotFound}
        let arguments = vm.peekAll()                        // 引数候補
        // TODO: - 引数をチェックし、実行する関数を特定し、Frameに割り当てる。
        guard arguments.count >= function.numberOfParameters else {
            return InputFormatError.numberOfParameters(function.numberOfParameters).message
        }
        vm.drop(function.numberOfParameters)
        let frame = Frame(with: closure, basePointer: vm.sp)
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
        return vm.push(returnValue)
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
        return vm.push(vm.constants[constIndex])
    }
}
struct PhraseExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() -> JpfError? {
        let particleIndex = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        if let value = vm.pull() {
            let p = Token.particles[particleIndex]
            let phrase = JpfPhrase(value: value, particle: Token(p))
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
        return vm.push(vm.globals[globalIndex])
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
        guard let getPropertyOf = ObjectProperties()[index]?.accessor else {
            return propertyNotFound
        }
        guard let object = vm.pull() else {
            return objectNotFound
        }
        guard let result = getPropertyOf(object) else {
            return failedToGetProperty
        }
        if result.isError {return result.error}
        return vm.push(result)
    }
}
struct PredicateExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() -> JpfError? {
        let index = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        let predicate = PredicateOperableFactory.predicates[index].operator
        let environment = Environment(with: vm.stack)   // スタックを入力として渡す
        switch predicate(environment).operated() {      // 述語を評価
        case .some(let result):
            return !result.isError ?                    // 評価結果をpush
            vm.push(result) :
            result.error
        case .none:
            return pushOutputValues(from: environment)  // 代入値をスタックに格納
        }
    }
}
private extension PredicateExecuter {
    /// 識別子に代入する値を、スタックに格納する。
    /// - Parameter env: 代入値を含む
    /// - Returns: nil : 正常、エラー(JpfError)
    func pushOutputValues(from env: Environment) -> JpfError? {
        // 代入する値が単一である場合、それをVMのスタックに格納
        if let value = env.singleValue {
            return vm.push(value)
        }
        // 複数の識別子に代入する値を取り出し、逆順でVMのスタックに格納
        for (_, value) in env.parameterPairs.reversed() {
            if value.isError {
                return value.error
            }
            if let error = vm.push(value) {
                return error
            }
        }
        return nil
    }
}
struct ClosureExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() -> JpfError? {
        let functionIndex = Int(readUInt16(from: bytes))
        let numberOfFreeVariables = Int(readUInt8(from: Array(bytes[2...])))
        vm.currentFrame.advanceIp(by: 3)
        return pushClosure(with: functionIndex, numberOfFreeVariables)
    }
}
private extension ClosureExecuter {
    func pushClosure(with index: Int, _ number: Int) -> JpfError? {
        guard let function = vm.constants[index] as? JpfCompiledFunction else {return failedToGetFunction}
        let freeVariables = (0..<number).compactMap {vm.stack[vm.sp - number + $0]}
        guard freeVariables.count == number else {return "\(number)個の" + failedToGetFreeVariables}
        vm.sp -= number         // 自由変数領域解放
        let closure = JpfClosure(with: function, freeVariables)
        return vm.push(closure)
    }
}
struct GetFreeExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() -> JpfError? {
        let index = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        guard let freeVariable = vm.currentFrame.cl?.free[index] else {return failedToGetClosure}
        return vm.push(freeVariable)
    }
}
struct CurrentClosureExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        guard let currentClosure = vm.currentFrame.cl else {return failedToGetClosure}
        return vm.push(currentClosure)
    }
}
