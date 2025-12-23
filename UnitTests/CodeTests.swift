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
            (.opConstant,   [65534],        [Opcode.opConstant.rawValue, 255, 254]),
            (.opReturn,     [],             [Opcode.opReturn.rawValue]),
            (.opGetLocal,   [255],          [Opcode.opGetLocal.rawValue, 255]),
            (.opClosure,    [65534, 255],   [Opcode.opClosure.rawValue, 255, 254, 255]),
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
            make(op: .opClosure,  operand: 65535, 255),
        ]
        let expected = """
        0000 OpReturn
        0001 OpGetLocal 1
        0003 OpConstant 2
        0006 OpConstant 65535
        0009 OpClosure 65535, 255

        """
        XCTAssertEqual(expected, Instructions(instructions).string, "instructions wrongly formatted.")
    }
    func testReadOperands() throws {
        let testPattern: [(op: Opcode, operands: [Int], bytesRead: Int)] = [
            (.opConstant, [65535], 2),
            (.opGetLocal, [255], 1),
            (.opClosure,  [65535, 255], 3)
        ]
        for test in testPattern {
            let instruction = make(op: test.op, operands: test.operands)
            let (opearndsRead, n) = test.op.readOperand(from: Array(instruction[1...]))
            XCTAssertEqual(n, test.bytesRead, "n wrong.")
            for (i, exptected) in test.operands.enumerated() {
                XCTAssertEqual(opearndsRead[i], exptected, "operand wrong.")
            }
        }
    }
    func testVmCodes() throws {
        let testPattern: [(instructions: [Instruction], expected: [Int])] = [
            ([  // test opArrayConst 2
                make(op: .opConstant, operand: 1),
                make(op: .opConstant, operand: 2),
                make(op: .opArrayConst, operand: 2),
            ], [1,2]),
            ([  // test opArray
                make(op: .opConstant, operand: 1),
                make(op: .opConstant, operand: 2),
                make(op: .opConstant, operand: 2),
                make(op: .opArray),
            ], [1,2]),
            ([  // test opPullConst 2
                make(op: .opConstant, operand: 1),
                make(op: .opConstant, operand: 2),
                make(op: .opPullConst, operand: 2),
                make(op: .opArrayConst, operand: 2),
            ], [1,2]),
            ([  // test opPull
                make(op: .opConstant, operand: 1),
                make(op: .opConstant, operand: 2),
                make(op: .opConstant, operand: 2),
                make(op: .opPull),
                make(op: .opArrayConst, operand: 2),
            ], [1,2]),
            ([  // test opDuplicateConst 2
                make(op: .opConstant, operand: 1),
                make(op: .opConstant, operand: 2),
                make(op: .opDuplicateConst, operand: 2),
                make(op: .opArrayConst, operand: 4),
            ], [1,2,1,2]),
            ([  // test opDuplicate
                make(op: .opConstant, operand: 1),
                make(op: .opConstant, operand: 2),
                make(op: .opConstant, operand: 2),
                make(op: .opDuplicate),
                make(op: .opArrayConst, operand: 4),
            ], [1,2,1,2]),
            ([  // test opMapProperty 値
                make(op: .opConstant, operand: 5),      // 1と
                make(op: .opConstant, operand: 2),      // 2
                make(op: .opArrayConst, operand: 2),
                make(op: .opMapProperty, operand: 2),   // 値
            ], [1,2]),
            ([  // test opMapProperty 数値
                make(op: .opConstant, operand: 5),      // 1と
                make(op: .opConstant, operand: 6),      // "2"
                make(op: .opArrayConst, operand: 2),
                make(op: .opMapProperty, operand: 3),   // 数値
            ], [1,2]),
        ]
        let testConstatnts: [JpfObject] = [
            JpfInteger(value: 0),
            JpfInteger(value: 1),
            JpfInteger(value: 2),
            JpfInteger(value: 3),
            JpfInteger(value: 4),
            JpfPhrase(value: JpfInteger(value: 1), particle: Token(.TO)),
            JpfString(value: "2"),
        ]
        for test in testPattern {
            let instruction = Instructions(test.instructions.flatMap {$0})
            print("テスト：\n\(instruction.string)")
            let vm = VM(with: Bytecode(instruction, testConstatnts))
            XCTAssertNil(vm.run())
            let array = try XCTUnwrap(vm.pull() as? JpfArray)
            let integers = try XCTUnwrap(array.elements as? [JpfInteger])
            let numbers = integers.map(\.value)
            XCTAssertEqual(numbers, test.expected)
            print("結果：\(numbers)")
        }
    }
}
