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
             ]
            ),
            (input: "１と２を。足す",
             expectedConstants: [1, 2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opAdd),
             ]
            ),
            (input: "１から２を引く",
             expectedConstants: [-1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
             ]
            ),
            (input: "１から２を。引く",
             expectedConstants: [1, 2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opSub),
             ]
            ),
            (input: "１と２を掛ける",
             expectedConstants: [2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
             ]
            ),
            (input: "１と２を。掛ける",
             expectedConstants: [1, 2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opMul),
             ]
            ),
            (input: "２を１で割る",
             expectedConstants: [2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
             ]
            ),
            (input: "２を１で。割る",
             expectedConstants: [2, 1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opDiv),
             ]
            ),
            (input: "１の負数",
             expectedConstants: [-1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
             ]
            ),
            (input: "-１を負数にする",
             expectedConstants: [1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
             ]
            ),
            (input: "１。負数",
             expectedConstants: [1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opNeg),
             ]
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
             ]
            ),
            (input: "偽",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
             ]
            ),
            (input: "1が2より大きい",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
             ]
            ),
            (input: "1が2より。大きい",
             expectedConstants: [1, 2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opGreaterThan),
             ]
            ),
            (input: "1が2より小さい",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opTrue),
             ]
            ),
            (input: "1が2より。小さい",
             expectedConstants: [1, 2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opLessThan),
             ]
            ),
            (input: "1が2に等しい",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
             ]
            ),
            (input: "1が2に。等しい",
             expectedConstants: [1, 2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opEqual),
             ]
            ),
            (input: "1が2に等しくない",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
                make(op: .opNot),
             ]
            ),
            (input: "1が2に。等しくない",
             expectedConstants: [1, 2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opEqual),
                make(op: .opNot),
             ]
            ),
            (input: "真が偽に等しい",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
             ]
            ),
            (input: "真が偽に。等しい",
             expectedConstants: [true, false],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opEqual),
             ]
            ),
            (input: "真が偽に等しくない",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
                make(op: .opNot),
             ]
            ),
            (input: "真が偽に。等しくない",
             expectedConstants: [true, false],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opEqual),
                make(op: .opNot),
             ]
            ),
            (input: "真でない",
             expectedConstants: [true],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opNot),
             ]
            ),
            (input: "真で。ない",
             expectedConstants: [true],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opNot),
             ]
            ),
            (input: "1が正",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opTrue),
             ]
            ),
            (input: "1。正",
             expectedConstants: [1, 0],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opGreaterThan),
             ]
            ),
            (input: "-1が負",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opTrue),
             ]
            ),
            (input: "-1。負",
             expectedConstants: [-1, 0],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opLessThan),
             ]
            ),
            (input: "0が正",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
             ]
            ),
            (input: "0。正",
             expectedConstants: [0, 0],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opGreaterThan),
             ]
            ),
        ]
        try runCompilerTests(testPatterns)
    }
    func testConditionals() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "真である場合、【１０】。３３３３。",
             expectedConstants: [true, 10, 3333],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),          // 0000 真
                make(op: .opBe),                            // 0003
                make(op: .opJumpNotTruthy, operand: 10),    // 0004
                make(op: .opConstant, operand: 1),          // 0007 10
                make(op: .opConstant, operand: 2),          // 0010 3333
             ]
            ),
            (input: "１が１に。等しい場合、【１０】。３３３３。",
             expectedConstants: [1, 1, 10, 3333],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),          // 0000 1
                make(op: .opConstant, operand: 1),          // 0003 1
                make(op: .opEqual),                         // 0006
                make(op: .opJumpNotTruthy, operand: 13),    // 0007
                make(op: .opConstant, operand: 2),          // 0010 10
                make(op: .opConstant, operand: 3),          // 0013 3333
             ]
            ),
            (input: "真である場合、【１０】、それ以外は、【２０】。３３３３。",
             expectedConstants: [true, 10, 20, 3333],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),          // 0000 真
                make(op: .opBe),                            // 0003
                make(op: .opJumpNotTruthy, operand: 13),    // 0004
                make(op: .opConstant, operand: 1),          // 0007 10
                make(op: .opJump, operand: 16),             // 0010
                make(op: .opConstant, operand: 2),          // 0013 20
                make(op: .opConstant, operand: 3),          // 0016 3333
             ]
            ),
            (input: "１が１に。等しい場合、【１０】、それ以外は、【２０】。３３３３。",
             expectedConstants: [1, 1, 10, 20, 3333],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),          // 0000 1
                make(op: .opConstant, operand: 1),          // 0003 1
                make(op: .opEqual),                         // 0006
                make(op: .opJumpNotTruthy, operand: 16),    // 0007
                make(op: .opConstant, operand: 2),          // 0010 10
                make(op: .opJump, operand: 19),             // 0013
                make(op: .opConstant, operand: 3),          // 0016 20
                make(op: .opConstant, operand: 4),          // 0019 3333
             ]
            ),
        ]
        try runCompilerTests(testPatterns)
    }
    func testGlobalDefStatements() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "一は１。二は2。",
             expectedConstants: [1, 2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opSetGlobal, operand: 1),
             ]
            ),
            (input: "一は１。一。",
             expectedConstants: [1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 0),
             ]
            ),
            (input: "一は１。二は一。二。",
             expectedConstants: [1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opSetGlobal, operand: 1),
                make(op: .opGetGlobal, operand: 1),
             ]
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
            case let constant as Bool:
                try testBooleanObject(constant, object)
            default:
                break
            }
        }
    }
    private func testIntegerObject(_ expected: Int64, _ actual: JpfObject) throws {
        let integer = try XCTUnwrap(actual.value as? JpfInteger)
        XCTAssertEqual(integer.value, Int(expected))
    }
    private func testBooleanObject(_ expected: Bool, _ actual: JpfObject) throws {
        let integer = try XCTUnwrap(actual.value as? JpfBoolean)
        XCTAssertEqual(integer.value, expected)
    }
}
extension String {
    var quoted: String {"\"\(self)\"".replacingOccurrences(of: "\n", with: "\\n")}
}
