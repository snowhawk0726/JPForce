//
//  Benchmark.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/11/16.
//

import XCTest

final class Benchmark : XCTestCase {
    func testFibonacci() {
        let paramenter = 25
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
        guard let program = parser.parseProgram(), parser.errors.isEmpty else {
            parser.errors.forEach {print("Parser errors: \($0)")}
            return
        }
        // interpriter
        let environment = Environment()
        let evaluator = Evaluator(from: program, with: environment)
        var start = Date()
        var object = evaluator.object
        var duration = Date().timeIntervalSince(start)
        guard let result = object?.number else {
            print("評価エラー：\(String(describing: object))")
            return
        }
        print("インタープリタ:\t結果は、\(result)、実行時間は、\(duration)秒")
        // compiler
        let compiler = Compiler(from: program)
        start = Date()
        if let error = compiler.compile() {
            print("コンパイルエラー：\(error.message)")
            return
        }
        duration = Date().timeIntervalSince(start)
        print("コンパイラ:\t\t実行時間は、\(duration)秒")
        // vm
        let vm = VM(with: compiler.bytecode)
        start = Date()
        if let error = vm.run() {
            print("実行エラー：\(error.message)")
            return
        }
        object = vm.stack.top
        duration = Date().timeIntervalSince(start)
        guard let result = object?.number else {
            print("エラー：\(String(describing: object))")
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
