//
//  repl.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/03/02.
//

import Foundation

struct Repl {
    func start() {
        let environment = Environment()
        print("日本語ふぉーすのREPLです。(評価結果表示バージョン)")
        while true {
            printPrompt()
            guard let line = readLine(), !line.isEmpty else {
                return
            }
            let lexer = Lexer(line)
            let parser = Parser(lexer)
            guard let program = parser.parseProgram(), parser.errors.isEmpty else {
                printErros(of: parser.errors)
                continue
            }
            let evaluated = program.evaluated(with: environment)
            evaluated.map {print("評価結果: \($0.string)")}
            print("入力: (\(environment.string))")
        }
    }
    private func printPrompt() {
        print(">> ", terminator: "")
    }
    private func printErros(of errors: [String]) {
        print("構文解析器が、\(errors.count)個のエラーを検出した。")
        errors.forEach {print("\t\($0)")}
    }
}
