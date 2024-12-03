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
        expectedInstructions: [Instruction]
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
             ]),
            (input: "１と２を。足す",
             expectedConstants: [(1,"と"), (2,"を")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(predicate: .ADD),      // 足す
             ]),
            (input: "１から２を引く",
             expectedConstants: [-1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
             ]),
            (input: "１から２を。引く",
             expectedConstants: [(1,"から"), (2,"を")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(predicate: .SUBSTRACT),// 引く
             ]),
            (input: "１と２を掛ける",
             expectedConstants: [2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
             ]),
            (input: "１と２を。掛ける",
             expectedConstants: [(1,"と"), (2,"を")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(predicate: .MULTIPLY), // 掛ける
             ]),
            (input: "２を１で割る",
             expectedConstants: [2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
             ]),
            (input: "２を１で。割る",
             expectedConstants: [(2,"を"), (1,"で")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(predicate: .DIVIDE),   // 割る
             ]),
            (input: "１の負数",
             expectedConstants: [-1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
             ]),
            (input: "-１を負数にする",
             expectedConstants: [1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
             ]),
            (input: "１。負数",
             expectedConstants: [1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(predicate: .NEGATE),   // 負数
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testBooleanExpressions() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "真",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opTrue),
             ]),
            (input: "偽",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
             ]),
            (input: "1が2より大きい",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
             ]),
            (input: "1が2より。大きい",
             expectedConstants: [(1,"が"),(2,"より")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(predicate: .GT),   // 大きい
             ]),
            (input: "1が2より小さい",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opTrue),
             ]),
            (input: "1が2より。小さい",
             expectedConstants: [(1,"が"),(2,"より")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(predicate: .LT),   // 小さい
             ]),
            (input: "1が2に等しい",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
             ]),
            (input: "1が2に。等しい",
             expectedConstants: [(1,"が"),(2,"に")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(predicate: .EQUAL),// 等しい
             ]),
            (input: "1が2に等しくない",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
                make(predicate: .NOT),  // ない
             ]),
            (input: "1が2に。等しくない",
             expectedConstants: [(1,"が"),(2,"に")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(predicate: .EQUAL),// 等しく
                make(predicate: .NOT),  // ない
             ]),
            (input: "真が偽に等しい",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
             ]),
            (input: "真が偽に。等しい",
             expectedConstants: [(true,"が"), (false,"に")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(predicate: .EQUAL),// 等しい
             ]),
            (input: "真が偽に等しくない",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
                make(predicate: .NOT),  // ない
             ]),
            (input: "真が偽に。等しくない",
             expectedConstants: [(true,"が"), (false,"に")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(predicate: .EQUAL),// 等しく
                make(predicate: .NOT),  // ない
             ]),
            (input: "真でない",
             expectedConstants: [(true,"で")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(predicate: .NOT),  // ない
             ]),
            (input: "真で。ない",
             expectedConstants: [(true,"で")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(predicate: .NOT),  // ない
             ]),
            (input: "1が正",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opTrue),
             ]),
            (input: "1。正",
             expectedConstants: [1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opGetProperty, operand: 8),
             ]),
            (input: "-1が負",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opTrue),
             ]),
            (input: "-1。負",
             expectedConstants: [-1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opGetProperty, operand: 9),
             ]),
            (input: "0が正",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opFalse),
             ]),
            (input: "0。正",
             expectedConstants: [0],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opGetProperty, operand: 8),
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testStringExpressions() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "「monkey」",
             expectedConstants: ["monkey"],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
             ]),
            (input: "「mon」と「key」を足す",
             expectedConstants: ["monkey"],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
             ]),
            (input: "「mon」と「key」を。足す",
             expectedConstants: [("mon","と"), ("key","を")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(predicate: .ADD),  // 足す
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testArrayLiterals() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "配列【】",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opArray, operand: 0),
             ]),
            (input: "配列【1、2、3】",
             expectedConstants: [1, 2, 3],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opConstant, operand: 2),
                make(op: .opArray, operand: 3),
             ]),
            (input: "配列【１と２を足す、３から４を引く、５と６を掛ける】",
             expectedConstants: [3, -1, 30],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),  // 1 + 2
                make(op: .opConstant, operand: 1),  // 3 - 4
                make(op: .opConstant, operand: 2),  // 5 * 6
                make(op: .opArray, operand: 3),
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testDictionaryLiterals() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "辞書【】",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opDictionary, operand: 0),
             ]),
            (input: "辞書【１が２、３が４、５が６】",
             expectedConstants: [1,2,3,4,5,6],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opConstant, operand: 2),
                make(op: .opConstant, operand: 3),
                make(op: .opConstant, operand: 4),
                make(op: .opConstant, operand: 5),
                make(op: .opDictionary, operand: 6),
             ]),
            (input: "辞書【１が２と３を足す、４が５と６を掛ける】",
             expectedConstants: [1, 5, 4, 30],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),  // 1が
                make(op: .opConstant, operand: 1),  // 2と3を足す
                make(op: .opConstant, operand: 2),  // 4が
                make(op: .opConstant, operand: 3),  // 5と６を掛ける
                make(op: .opDictionary, operand: 4),
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testIndexExpressions() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "iは、1と1を足す。配列【１、２、３】のi",
             expectedConstants: [2, 1, 2, 3],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opConstant, operand: 2),
                make(op: .opConstant, operand: 3),
                make(op: .opArray, operand: 3),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opGenitive),
             ]),
            (input: "配列【１、２、３】の２番目",
             expectedConstants: [3],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
             ]),
            (input: "aは、配列【１、２、３】。iは２番目。aのi",
             expectedConstants: [1, 2, 3, 2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opConstant, operand: 2),
                make(op: .opArray, operand: 3),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opConstant, operand: 3),
                make(op: .opSetGlobal, operand: 1),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 1),
                make(op: .opGenitive),
             ]),
            (input: "iは、2から1を引く。辞書【１が２】のi",
             expectedConstants: [1, 1, 2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opConstant, operand: 2),
                make(op: .opDictionary, operand: 2),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opGenitive),
             ]),
            (input: "辞書【１が２】の１",
             expectedConstants: [2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testPhraseExpressions() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "１と２を。足す。", // 句は定数
             expectedConstants: [(1, "と"), (2, "を")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),  // 1と
                make(op: .opConstant, operand: 1),  // 2を
                make(predicate: .ADD),              // 足す
            ]),
            (input: "aは１。bは２。aとbを足す。",
             expectedConstants: [1, 2],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),  // 1
                make(op: .opSetGlobal, operand: 0), // a
                make(op: .opConstant, operand: 1),  // 2
                make(op: .opSetGlobal, operand: 1), // b
                make(op: .opGetGlobal, operand: 0), // a
                make(phraseWith: .TO),              // 「と」から「aと」を作る
                make(op: .opGetGlobal, operand: 1), // b
                make(phraseWith: .WO),              // 「を」から「bを」を作る
                make(predicate: .ADD),              // 足す
            ]),
            (input: "関数【】を実行する。",
             expectedConstants: [
                make(op: .opReturn),
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 0, 0),
                make(op: .opCall),                  // 「を」は取り除かれる
            ]),
            (input: "テストは関数【】。テストをする。",
             expectedConstants: [
                make(op: .opReturn),
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 0, 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opCall),                  // 「を」は取り除かれる
            ]),
            (input: "1を負数にする。",
             expectedConstants: [-1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
            ]),
            (input: "1を。負数にする。",
             expectedConstants: [(1, "を")],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(predicate: .NEGATE),           // 「に(する）」は取り除かれる
            ]),
            (input: "１と２を。足したものに、3と４を。足したものを、掛ける。",
             expectedConstants: [
                (1, "と"),
                (2, "を"),
                (3, "と"),
                (4, "を"),
             ],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),  // 1と
                make(op: .opConstant, operand: 1),  // 2を
                make(predicate: .ADD),              // 足し
                make(phraseWith: .NI),              // に (直前の「た」は取り除く
                make(op: .opConstant, operand: 2),  // 3と
                make(op: .opConstant, operand: 3),  // 4を
                make(predicate: .ADD),              // 足し
                make(phraseWith: .WO),              // を (直前の「た」は取り除く
                make(predicate: .MULTIPLY),         // 掛ける
            ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testConditionals() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "真である場合、【１０】。３３３３。",
             expectedConstants: [(true,"で"), 10, 3333],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),          // 0000 真で
                make(predicate: .BE),                       // 0003 ある
                make(op: .opJumpNotTruthy, operand: 11),    // 0005
                make(op: .opConstant, operand: 1),          // 0008 10
                make(op: .opConstant, operand: 2),          // 0011 3333
             ]),
            (input: "１が１に。等しい場合、【１０】。３３３３。",
             expectedConstants: [(1,"が"), (1,"に"), 10, 3333],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),          // 0000 1
                make(op: .opConstant, operand: 1),          // 0003 1
                make(predicate: .EQUAL),                    // 0006 等しい
                make(op: .opJumpNotTruthy, operand: 14),    // 0008
                make(op: .opConstant, operand: 2),          // 0011 10
                make(op: .opConstant, operand: 3),          // 0014 3333
             ]),
            (input: "真である場合、【１０】、それ以外は、【２０】。３３３３。",
             expectedConstants: [(true,"で"), 10, 20, 3333],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),          // 0000 真で
                make(predicate: .BE),                       // 0003 ある
                make(op: .opJumpNotTruthy, operand: 14),    // 0005
                make(op: .opConstant, operand: 1),          // 0008 10
                make(op: .opJump, operand: 17),             // 0011
                make(op: .opConstant, operand: 2),          // 0014 20
                make(op: .opConstant, operand: 3),          // 0017 3333
             ]),
            (input: "１が１に。等しい場合、【１０】、それ以外は、【２０】。３３３３。",
             expectedConstants: [(1,"が"), (1,"に"), 10, 20, 3333],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),          // 0000 1が
                make(op: .opConstant, operand: 1),          // 0003 1に
                make(predicate: .EQUAL),                    // 0006
                make(op: .opJumpNotTruthy, operand: 17),    // 0008
                make(op: .opConstant, operand: 2),          // 0011 10
                make(op: .opJump, operand: 20),             // 0014
                make(op: .opConstant, operand: 3),          // 0017 20
                make(op: .opConstant, operand: 4),          // 0020 3333
             ]),
            (input: "１が。１である場合、【１０】、それ以外は、【２０】。３３３３。",
             expectedConstants: [(1,"が"), (1,"で"), 10, 20, 3333],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),          // 0000 1が
                make(op: .opConstant, operand: 1),          // 0003 1で
                make(predicate: .BE),                       // 0006 ある
                make(op: .opJumpNotTruthy, operand: 17),    // 0008
                make(op: .opConstant, operand: 2),          // 0011 10
                make(op: .opJump, operand: 20),             // 0014
                make(op: .opConstant, operand: 3),          // 0017 20
                make(op: .opConstant, operand: 4),          // 0020 3333
             ]),
            (input: "２が。１の場合、【１０】、２の場合、【２０】、それ以外は、【３０】。３３３３。",
             expectedConstants: [(2,"が"), (1,"で"), 10, (2,"で"), 20, 30, 3333],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),          // 0000 2が
                make(predicate: .DUPLICATE),                // 0003 写す
                make(op: .opConstant, operand: 1),          // 0005 1で
                make(predicate: .BE),                       // 0008 ある
                make(op: .opJumpNotTruthy, operand: 21),    // 0010 場合
                make(predicate: .DROP),                     // 0013 捨てる
                make(op: .opConstant, operand: 2),          // 0015 10
                make(op: .opJump, operand: 44),             // 0018 】
                make(predicate: .DUPLICATE),                // 0021 写す
                make(op: .opConstant, operand: 3),          // 0023 2で
                make(predicate: .BE),                       // 0026 ある
                make(op: .opJumpNotTruthy, operand: 39),    // 0028 場合
                make(predicate: .DROP),                     // 0031 捨てる
                make(op: .opConstant, operand: 4),          // 0033 20
                make(op: .opJump, operand: 44),             // 0036 】
                make(predicate: .DROP),                     // 0039 捨てる
                make(op: .opConstant, operand: 5),          // 0041 30
                make(op: .opConstant, operand: 6),          // 0044 3333
             ]),
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
             ]),
            (input: "一は１。一。",
             expectedConstants: [1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 0),
             ]),
            (input: "一は１。二は一。二。",
             expectedConstants: [1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opSetGlobal, operand: 1),
                make(op: .opGetGlobal, operand: 1),
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testFunctions() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "関数【５と１０を足し、返す】",
             expectedConstants: [
                15,
                [
                    make(op: .opConstant, operand: 0),
                    make(op: .opReturnValue),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 1, 0),
             ]),
            (input: "関数【５と１０を足す】",
             expectedConstants: [
                15,
                [
                    make(op: .opConstant, operand: 0),
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 1, 0),
             ]),
            (input: "関数【１。２】",
             expectedConstants: [
                1,
                2,
                [
                    make(op: .opConstant, operand: 0),
                    make(op: .opConstant, operand: 1),
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 2, 0),
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testFunctionsWithoutReturnValue() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "関数【】",
             expectedConstants: [
                [
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 0, 0),
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testCompilerScopes() throws {
        let compiler = Compiler(from: Program())
        XCTAssertEqual(compiler.scopeIndex, 0)
        let globalSymbolTable = SymbolTable()
        _ = compiler.emit(op: .opTrue)
        //
        compiler.enterScope()
        XCTAssertEqual(compiler.scopeIndex, 1)
        _ = compiler.emit(op: .opFalse)
        XCTAssertEqual(compiler.scopes[compiler.scopeIndex].instructions.count, 1)
        var last = compiler.scopes[compiler.scopeIndex].lastInstruction
        XCTAssertEqual(last?.opcode, .opFalse)
        XCTAssertEqual(compiler.symbolTable.outer, globalSymbolTable)
        _ = compiler.leaveScope()
        //
        XCTAssertEqual(compiler.scopeIndex, 0)
        XCTAssertEqual(compiler.symbolTable, globalSymbolTable)
        XCTAssertEqual(compiler.symbolTable.outer, nil)
        _ = compiler.emit(op: .opNull)
        XCTAssertEqual(compiler.scopes[compiler.scopeIndex].instructions.count, 2)
        last = compiler.scopes[compiler.scopeIndex].lastInstruction
        XCTAssertEqual(last?.opcode, .opNull)
        let previous = compiler.scopes[compiler.scopeIndex].previousInstruction
        XCTAssertEqual(previous?.opcode, .opTrue)
    }
    func testFunctionCalls() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "関数【２４を返す】を実行",
             expectedConstants: [
                24,
                [
                    make(op: .opConstant, operand: 0),
                    make(op: .opReturnValue),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 1, 0),
                make(op: .opCall),
             ]),
            (input: "引数無は、関数【２４を返す】。引数無を実行する。",
             expectedConstants: [
                24,
                [
                    make(op: .opConstant, operand: 0),
                    make(op: .opReturnValue),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 1, 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opCall),
             ]),
            (input: "テストは、関数【２４】。テストする。",
             expectedConstants: [
                24,
                [
                    make(op: .opConstant, operand: 0),
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 1, 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opCall),
             ]),
            (input: "引数１は、関数【入力がa】。24で、引数１を実行する。",
             expectedConstants: [
                [
                    make(op: .opReturn),
                ],
                (24, "で"),
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 0, 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opCall),
             ]),
            (input: "引数３は、関数【入力がaとbとc】。24と25と26で、引数３を実行する。",
             expectedConstants: [
                [
                    make(op: .opReturn),
                ],
                (24, "と"),
                (25, "と"),
                (26, "で"),
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 0, 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opConstant, operand: 2),
                make(op: .opConstant, operand: 3),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opCall),
             ]),
            (input: "引数１は、関数【入力がa。a】。24で、引数１を実行する。",
             expectedConstants: [
                [
                    make(op: .opGetLocal, operand: 0),
                    make(op: .opReturn),
                ],
                (24, "で"),
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 0, 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opCall),
             ]),
            (input: "引数３は、関数【入力がaとbとc。a。b。c】。24と25と26で、引数３を実行する。",
             expectedConstants: [
                [
                    make(op: .opGetLocal, operand: 0),
                    make(op: .opGetLocal, operand: 1),
                    make(op: .opGetLocal, operand: 2),
                    make(op: .opReturn),
                ],
                (24, "と"),
                (25, "と"),
                (26, "で"),
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 0, 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opConstant, operand: 1),
                make(op: .opConstant, operand: 2),
                make(op: .opConstant, operand: 3),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opCall),
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testDefineStatementScopes() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "数字は５５。関数【数字】",
             expectedConstants: [
                55,
                [
                    make(op: .opGetGlobal, operand: 0),
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opClosure, operand: 1, 0),
             ]),
            (input: "関数【数字は５５。数字】",
             expectedConstants: [
                55,
                [
                    make(op: .opConstant, operand: 0),
                    make(op: .opSetLocal, operand: 0),
                    make(op: .opGetLocal, operand: 0),
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 1, 0),
             ]),
            (input: "関数【aは５５。bは７７。aとbを足す】",
             expectedConstants: [
                55,
                77,
                [
                    make(op: .opConstant, operand: 0),
                    make(op: .opSetLocal, operand: 0),
                    make(op: .opConstant, operand: 1),
                    make(op: .opSetLocal, operand: 1),
                    make(op: .opGetLocal, operand: 0),
                    make(phraseWith: .TO),
                    make(op: .opGetLocal, operand: 1),
                    make(phraseWith: .WO),
                    make(predicate: .ADD),
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 2, 0),
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testBuiltins() throws {
        let testPtterns: [CompilerTestCase] = [
            (input: "配列【】の数。配列【】に１を追加",
             expectedConstants: [0,1],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),      // 0
                make(op: .opConstant, operand: 1),      // 1
                make(op: .opArray, operand: 1),         // 配列【1】
             ]),
            (input: "配列【】。数。配列【】。１を追加",
             expectedConstants: [(1, "を")],
             expectedInstructions: [
                make(op: .opArray, operand: 0),         // 配列【】。
                make(op: .opGetProperty, operand: 6),   // 数
                make(op: .opArray, operand: 0),         // 配列【】。
                make(op: .opConstant, operand: 0),      // 1を
                make(predicate: .APPEND),               // 追加
             ]),
            (input: "関数【配列【】の数】",
             expectedConstants: [
                0,
                [
                    make(op: .opConstant, operand: 0),  // 配列【】の数 = 0
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 1, 0),    // 関数
             ]),
            (input: "関数【配列【】。数】",
             expectedConstants: [
                [
                    make(op: .opArray, operand: 0),     // 配列【】。
                    make(op: .opGetProperty, operand: 6),// 数
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 0, 0),     // 関数
             ]),
        ]
        try runCompilerTests(testPtterns)
    }
    func testClosures() throws {
        let testPtterns: [CompilerTestCase] = [
            (input: "関数【入力がa。関数【入力がb。aとbを足す】】",
             expectedConstants: [
                [
                    make(op: .opGetFree, operand: 0),   // a
                    make(phraseWith: .TO),              // と
                    make(op: .opGetLocal, operand: 0),  // b
                    make(phraseWith: .WO),              // を
                    make(predicate: .ADD),              // 足す
                    make(op: .opReturn),
                ],
                [
                    make(op: .opGetLocal, operand: 0),  // a
                    make(op: .opClosure, operand: 0, 1),// 関数【入力がb。...
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 1, 0),    // 関数【入力がa。...
             ]),
            (input: "関数【入力がa。関数【入力がb。関数【入力がc。aとbとcを足す】】】",
             expectedConstants: [
                [
                    make(op: .opGetFree, operand: 0),   // a
                    make(phraseWith: .TO),              // と
                    make(op: .opGetFree, operand: 1),   // b
                    make(phraseWith: .TO),              // と
                    make(op: .opGetLocal, operand: 0),  // c
                    make(phraseWith: .WO),              // を
                    make(predicate: .ADD),              // 足す
                    make(op: .opReturn),
                ],
                [
                    make(op: .opGetFree, operand: 0),   // a
                    make(op: .opGetLocal, operand: 0),  // b
                    make(op: .opClosure, operand: 0, 2),// 関数【入力がc。...
                    make(op: .opReturn),
                ],
                [
                    make(op: .opGetLocal, operand: 0),  // a
                    make(op: .opClosure, operand: 1, 1),// 関数【入力がb。...
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 2, 0),    // 関数【入力がa。...
             ]),
            (input: """
                globalは、55。
                関数【aは、66。
                    関数【bは、77。
                        関数【cは、88。globalと、aとbとcを足す】
                    】
                】
            """,
             expectedConstants: [
                55, 66, 77, 88,
                [
                    make(op: .opConstant, operand: 3),  // 88
                    make(op: .opSetLocal, operand: 0),  // c
                    make(op: .opGetGlobal, operand: 0), // global(55)
                    make(phraseWith: .TO),              // と
                    make(op: .opGetFree, operand: 0),   // a
                    make(phraseWith: .TO),              // と
                    make(op: .opGetFree, operand: 1),   // b
                    make(phraseWith: .TO),              // と
                    make(op: .opGetLocal, operand: 0),  // c
                    make(phraseWith: .WO),              // を
                    make(predicate: .ADD),              // 足す
                    make(op: .opReturn),
                ],
                [
                    make(op: .opConstant, operand: 2),  // 77
                    make(op: .opSetLocal, operand: 0),  // b
                    make(op: .opGetFree, operand: 0),   // a
                    make(op: .opGetLocal, operand: 0),  // b
                    make(op: .opClosure, operand: 4, 2),// 関数【cは、88。...
                    make(op: .opReturn),
                ],
                [
                    make(op: .opConstant, operand: 1),  // 66
                    make(op: .opSetLocal, operand: 0),  // a
                    make(op: .opGetLocal, operand: 0),  // a
                    make(op: .opClosure, operand: 5, 1),// 関数【bは、77。...
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opConstant, operand: 0),      // 55
                make(op: .opSetGlobal, operand: 0),     // global
                make(op: .opClosure, operand: 6, 0),    // 関数【aは、66。...
             ]),
        ]
        try runCompilerTests(testPtterns)
    }
    func testRecursiveFunctions() throws {
        let tests: [CompilerTestCase] = [
            (input: "countDownは、関数【入力がx。xから1を引き、countDownする】。１をcountDownする。",
             expectedConstants: [
                (1,"を"),
                [
                    make(op: .opGetLocal, operand: 0),
                    make(phraseWith: .KARA),
                    make(op: .opConstant, operand: 0),
                    make(predicate: .SUBSTRACT),
                    make(op: .opCurrentClosure),
                    make(op: .opCall),
                    make(op: .opReturn),
                ],
                (1,"を"),
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 1, 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opConstant, operand: 2),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opCall),
             ]
            ),
            (input: "wrapperは、関数【countDownは、関数【入力がx。xから1を引き、countDownする】。1をcountDownする】。wrapperを実行。",
             expectedConstants: [
                (1,"を"),
                [
                    make(op: .opGetLocal, operand: 0),
                    make(phraseWith: .KARA),
                    make(op: .opConstant, operand: 0),
                    make(predicate: .SUBSTRACT),
                    make(op: .opCurrentClosure),
                    make(op: .opCall),
                    make(op: .opReturn),
                ],
                (1,"を"),
                [
                    make(op: .opClosure, operand: 1, 0),
                    make(op: .opSetLocal, operand: 0),
                    make(op: .opConstant, operand: 2),
                    make(op: .opGetLocal, operand: 0),
                    make(op: .opCall),
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 3, 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opCall),
             ]
            ),
        ]
        try runCompilerTests(tests)
    }
    // MARK: - Helpers
    private func runCompilerTests(_ tests: [CompilerTestCase]) throws {
        for t in tests {
            print("テスト開始：「\(t.input)」")
            let program = parseProgram(with: t.input)!
            let compiler = Compiler(from: program)
            XCTAssertNil(compiler.compile())
            let bytecode = compiler.bytecode
            testInstructions(t.expectedInstructions, bytecode.instructions)
            try testConstants(t.expectedConstants, bytecode.constants)
            print("テスト終了：\(bytecode.instructions.string.quoted)")
        }
    }
    private func testInstructions(_ expected: [Instruction], _ actual: Instructions) {
        let instructions = Instructions(expected)
        XCTAssertEqual(instructions.count, actual.count,
                       "関数「\(#function)」で、インストラクション長が違う。\n期待は、\(instructions.string.quoted)\n実際は、\(actual.string.quoted)")
        for (i, (expectedByte, actualByte)) in zip(instructions.bytes, actual.bytes).enumerated() {
            XCTAssertEqual(expectedByte, actualByte,
                           "関数「\(#function)」で、\(i)番地のインストラクションが異なる。\n期待は、\(instructions.string.quoted)\n実際は、\(actual.string.quoted)")
        }
    }
    private func testConstants(_ expected: [Any], _ actual: [JpfObject]) throws {
        XCTAssertEqual(expected.count, actual.count,
                       "関数「\(#function)」で、定数の数が誤っている。期待値は\(expected.count)だが、実際値は\(actual.count)。")
        for (expected, actual) in zip(expected, actual) {
            switch expected {
            case let integer as Int:
                try testIntegerObject(Int64(integer), actual)
            case let boolean as Bool:
                try testBooleanObject(boolean, actual)
            case let string as String:
                try testStringObject(string, actual)
            case let instructions as [Instruction]:
                let fn = try XCTUnwrap(actual as? JpfCompiledFunction)
                testInstructions(instructions, fn.instructions)
            case let (number, particle) as (Int?, String):
                let phrase = actual as? JpfPhrase
                if let n = number {
                    let object = try XCTUnwrap(phrase?.value)
                    try testIntegerObject(Int64(n), object)
                }
                let actualParticle = try XCTUnwrap(phrase?.particle?.literal)
                XCTAssertEqual(actualParticle, particle)
            default:
                break
            }
        }
    }
    private func testIntegerObject(_ expected: Int64, _ actual: JpfObject) throws {
        let integer = try XCTUnwrap(actual as? JpfInteger, "実際は、\(actual.type)")
        XCTAssertEqual(integer.value, Int(expected))
    }
    private func testBooleanObject(_ expected: Bool, _ actual: JpfObject) throws {
        let integer = try XCTUnwrap(actual as? JpfBoolean, "実際は、\(actual.type)")
        XCTAssertEqual(integer.value, expected)
    }
    private func testStringObject(_ expected: String, _ actual: JpfObject) throws {
        let string = try XCTUnwrap(actual as? JpfString, "実際は、\(actual.type)")
        XCTAssertEqual(string.value, expected)
    }
}
extension String {
    var quoted: String {"\"\(self)\"".replacingOccurrences(of: "\n", with: "\\n")}
}
