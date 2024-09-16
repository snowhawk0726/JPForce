//
//  CompilerTests.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/06/19.
//

import XCTest

final class CompilerTests: XCTestCase {
    typealias CompilerTestCase = (
        input: String,
        expectedConstants: [Any],
        expectedInstructions: [Instructions]
    )
    override func setUpWithError() throws {
    }
    override func tearDownWithError() throws {
    }
    func testIntegerArithmetic() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "１と２を足す",
             expectedConstants: [3],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opPop),  ]
            ),
            (input: "１から２を引く",
             expectedConstants: [-1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opPop),  ]
            ),
            (input: "１と２を掛ける",
             expectedConstants: [2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opPop),  ]
            ),
            (input: "２を１で割る",
             expectedConstants: [2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opPop),  ]
            ),
            (input: "１の負数",
             expectedConstants: [-1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opPop),  ]
            ),
            (input: "-１を負数にする",
             expectedConstants: [1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opPop),  ]
            ),
        ]
        try runCompilerTests(testPatterns)
    }
    func testBooleanExpressions() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "真",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opTrue),
                make(op: .opPop),  ]
            ),
            (input: "偽",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
                make(op: .opPop),  ]
            ),
            (input: "1が2より大きい",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
                make(op: .opPop),  ]
            ),
            (input: "1が2より小さい",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opTrue),
                make(op: .opPop),  ]
            ),
            (input: "1が2に等しい",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
                make(op: .opPop),  ]
            ),
            (input: "1が2に等しくない",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opTrue),
                make(op: .opPop),  ]
            ),
            (input: "真が偽に等しい",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
                make(op: .opPop),  ]
            ),
            (input: "真が偽に等しくない",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opTrue),
                make(op: .opPop),  ]
            ),
            (input: "真でない",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
                make(op: .opPop),  ]
            ),
            (input: "1が正",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opTrue),
                make(op: .opPop),  ]
            ),
            (input: "-1が負",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opTrue),
                make(op: .opPop),  ]
            ),
            (input: "0が正",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
                make(op: .opPop),  ]
            ),
        ]
        try runCompilerTests(testPatterns)
    }
    // MARK: - Helpers
    private func runCompilerTests(_ tests: [CompilerTestCase]) throws {
        for t in tests {
            let program = parseProgram(with: t.input)
            let compiler = Compiler(from: program)
            XCTAssertNil(compiler.compile())
            let bytecode = compiler.bytecode
            testInstructions(t.expectedInstructions, bytecode.instructions)
            try testConstants(t.expectedConstants, bytecode.constants)
        }
    }
    private func testInstructions(_ expected: [Instructions], _ actual: Instructions) {
        let concatted = concatInstructions(expected)
        XCTAssertEqual(actual.count, concatted.count,
                       "In \(#function): wrong instructions length.\nwant=\(concatted.string.quoted)\ngot= \(actual.string.quoted)")
        for (i, ins) in actual.enumerated() {
            XCTAssertEqual(ins, concatted[i],
                           "In \(#function): wrong instruction at \(i).\nwant=\(concatted.string.quoted)\ngot= \(actual.string.quoted)")
        }
    }
    private func concatInstructions(_ s: [Instructions]) -> Instructions {
        s.reduce(Instructions()) {$0 + $1}
    }
    private func testConstants(_ expected: [Any], _ actual: [JpfObject]) throws {
        XCTAssertEqual(expected.count, actual.count,
                       "In \(#function): wrong number of constants. got=\(String(describing: actual.count)), want=\(expected.count)")
        for (i, object) in actual.enumerated() {
            switch expected[i] {
            case let constant as Int:
                try testIntegerObject(Int64(constant), object)
            default:
                break
            }
        }
    }
    private func testIntegerObject(_ expected: Int64, _ actual: JpfObject) throws {
        let integer = try XCTUnwrap(actual as? JpfInteger)
        XCTAssertEqual(integer.value, Int(expected))
    }
}
extension String {
    var quoted: String {"\"\(self)\"".replacingOccurrences(of: "\n", with: "\\n")}
}
