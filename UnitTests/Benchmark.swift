//
//  Benchmark.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/11/16.
//

import XCTest
import Testing

final class Benchmark : XCTestCase {
    func testFibonacci() {
        let paramenter = 35
        print("パラメータは、\(paramenter)。")
        let input = """
        fibonacciは、関数【入力がx。
            xが、
            0の場合、0を返し、
            1の場合、1を返し、
            それ以外は、【
                xから1を引き、fibonacciを実行したものと、
                xから2を引き、fibonacciを実行したものを足し、
                返す。
            】
        】
        \(paramenter)で、fibonacciを実行する。
        """
        func fibonacci(_ x: Int) -> Int {
            switch x {
            case 0: return 0
            case 1: return 1
            default:
                return fibonacci(x - 1) + fibonacci(x - 2)
            }
        }
        let lexer = Lexer(input)
        let parser = Parser(lexer)
        parser.options.useSentenceAST = true
        guard let program = parser.parseProgram(), parser.errors.isEmpty else {
            parser.errors.forEach {print("Parser errors: \($0)")}
            return
        }
        // interpriter
        let environment = Environment()
        let evaluator = Evaluator(from: program, with: environment)
        var start = Date()
        var object = evaluator.object ?? environment.pull()
        var duration = Date().timeIntervalSince(start)
        guard let result = object?.number else {
            XCTFail("評価エラー：\(String(describing: object))")
            return
        }
        print("インタープリタ:\t結果は、\(result)、実行時間は、\(duration)秒")
        // compiler
        let compiler = Compiler(from: program)
        compiler.optimizeConstantsEnabled = false
        start = Date()
        if let error = compiler.compile() as? JpfError {
            XCTFail("コンパイルエラー：\(error.message)")
            return
        }
        duration = Date().timeIntervalSince(start)
        print("コンパイラ:\t\t実行時間は、\(duration)秒")
        // vm
        let vm = VM(with: compiler.bytecode)
        start = Date()
        if let error = vm.run() {
            XCTFail("実行エラー：\(error.message)")
            return
        }
        object = vm.stack.top
        duration = Date().timeIntervalSince(start)
        guard let result = object?.number else {
            XCTFail("エラー：\(String(describing: object))")
            return
        }
        print("VM:\t\t\t\t結果は、\(result)、実行時間は、\(duration)秒")
        // swift
        start = Date()
        let number = fibonacci(paramenter)
        duration = Date().timeIntervalSince(start)
        print("Swift:\t\t\t結果は、\(number)、実行時間は、\(duration)秒")
    }
}
struct BenchmarkTests {
    @Test(arguments: [100_000_000])
    func testOpAdd(_ n: Int) async throws {
        let constants: [JpfObject] = [
            JpfInteger(value: 1),
            JpfInteger(value: 2),
        ]
        let instructions: [Instruction] = [
            make(op: .opConstant, operand: 0),
            make(op: .opConstant, operand: 1),
            make(op: .opAdd, operand: 0),
            make(op: .opDrop),
        ]
        let bytecode = Bytecode(Instructions(instructions), constants)
        let vm = VM(with: bytecode)
        let start = Date()
        for _ in 0..<n {
            _ = vm.run()
        }
        let duration = Date().timeIntervalSince(start)
        print("実行時間は、\(duration)秒(\(n.formatted())回)")
        // 2026/8/6:   実行時間は、13.636877059936523秒(100,000,000回)
        // 2026/09/17: 実行時間は、3.338766932487488秒(100,000,000回) ← Release版
        print("Value size (int(Int))")
        print("    size:   \(MemoryLayout<JpfObject>.size)")
        print("    stride: \(MemoryLayout<JpfObject>.stride)")
        print("    align:  \(MemoryLayout<JpfObject>.alignment)")
    }

}
