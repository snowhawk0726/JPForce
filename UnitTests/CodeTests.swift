//
//  CodeTests.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/06/18.
//

import XCTest

final class CodeTests: XCTestCase {
    override func setUpWithError() throws {
    }
    override func tearDownWithError() throws {
    }
    func testMake() throws {
        let testPatterns: [(op: Opcode, operands: [Int], expected: [Byte]) ] = [
            (.opConstant, [65534], [Opcode.opConstant.rawValue, 255, 254]),
            (.opReturn, [], [Opcode.opReturn.rawValue]),
            (.opGetLocal, [255], [Opcode.opGetLocal.rawValue, 255]),
        ]
        for test in testPatterns {
            let instruction = make(op: test.op, operands: test.operands)
            XCTAssertEqual(instruction.count, test.expected.count,
                           "instruction has wrong length. want=\(test.expected.count), got=\(instruction.count)")
            for (i, b) in test.expected.enumerated() {
                XCTAssertEqual(instruction[i], test.expected[i],
                               "wrong byte at pos \(i). want=\(b.quoted), got=\(instruction[i].quoted)")
            }
        }
    }
    func testInstructionsString() throws {
        let instructions: [Instruction] = [
            make(op: .opReturn),
            make(op: .opGetLocal, operand: 1),
            make(op: .opConstant, operand: 2),
            make(op: .opConstant, operand: 65535),
        ]
        let expected = """
        0000 OpReturn
        0001 OpGetLocal 1
        0003 OpConstant 2
        0006 OpConstant 65535
        
        """
        XCTAssertEqual(Instructions(instructions).string, expected, "instructions wrongly formatted.")
    }
    func testReadOperands() throws {
        let testPattern: [(op: Opcode, operands: [Int], bytesRead: Int)] = [
            (.opConstant, [65535], 2),
            (.opGetLocal, [255], 1),
        ]
        for test in testPattern {
            let instruction = make(op: test.op, operands: test.operands)
            let (opearndsRead, n) = test.op.readOperands(with: Array(instruction[1...]))
            XCTAssertEqual(n, test.bytesRead, "n wrong.")
            for (i, exptected) in test.operands.enumerated() {
                XCTAssertEqual(opearndsRead[i], exptected, "operand wrong.")
            }
        }
    }
}
