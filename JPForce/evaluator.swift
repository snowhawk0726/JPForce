//
//  evaluator.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/03/06.
//

import Foundation

struct Evaluator {
    let node: Node              // ASTノード
    let environment: Environment// 評価環境
    init(from node: Node, with environment: Environment) {self.node = node; self.environment = environment}
    var object: JpfObject? {    // 評価結果: オブジェクトまたはnil(評価失敗)
        node.evaluate(with: environment)
    }
}
// MARK: - For evaluator tests
func testEvaluator(_ input: String) -> JpfObject? {
    let lexer = Lexer(input)
    let parser = Parser(lexer)
    guard let program = parser.parseProgram(), parser.errors.isEmpty else {
        parser.errors.forEach {print("Parser errors: \($0)")}
        return nil
    }
    let environment = Environment()
    let eval = Evaluator(from: program, with: environment)
    return eval.object ?? environment.peek
}
