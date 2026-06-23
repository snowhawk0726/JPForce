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
             expectedConstants: [(1,"と"), (2,"を")],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(predicate: .ADD),      // 足す
             ]),
            (input: "１から２を引く",
             expectedConstants: [(1,"から"), (2,"を")],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(predicate: .SUBSTRACT),// 引く
             ]),
            (input: "１と２を掛ける",
             expectedConstants: [(1,"と"), (2,"を")],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(predicate: .MULTIPLY), // 掛ける
             ]),
            (input: "２を１で割る",
             expectedConstants: [(2,"を"), (1,"で")],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(predicate: .DIVIDE),   // 割る
             ]),
            (input: "１の負数",
             expectedConstants: [1],
             expectedInstructions: [
                make(constant: 0),
                make(predicate: .NEGATE),   // 負数
             ]),
            (input: "-１を負数にする",
             expectedConstants: [(-1,"を")],
             expectedInstructions: [
                make(constant: 0),
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
             expectedConstants: [(1,"が"),(2,"より")],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(predicate: .GT),   // 大きい
             ]),
            (input: "1が2より小さい",
             expectedConstants: [(1,"が"),(2,"より")],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(predicate: .LT),   // 小さい
             ]),
            (input: "1が2に等しい",
             expectedConstants: [(1,"が"),(2,"に")],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(predicate: .EQUAL),// 等しい
             ]),
            (input: "1が2に等しくない",
             expectedConstants: [(1,"が"),(2,"に")],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(predicate: .EQUAL),// 等しく
                make(predicate: .NOT),  // ない
             ]),
            (input: "真が偽に等しい",
             expectedConstants: [(true,"が"), (false,"に")],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(predicate: .EQUAL),// 等しい
             ]),
            (input: "真が偽に等しくない",
              expectedConstants: [(true,"が"), (false,"に")],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(predicate: .EQUAL),// 等しく
                make(predicate: .NOT),  // ない
             ]),
            (input: "真でない",
             expectedConstants: [(true,"で")],
             expectedInstructions: [
                make(constant: 0),
                make(predicate: .NOT),  // ない
             ]),
            (input: "1が正",
             expectedConstants: [(1,"が")],
             expectedInstructions: [
                make(constant: 0),
                make(property: "正"),
             ]),
            (input: "-1が負",
             expectedConstants: [(-1,"が")],
             expectedInstructions: [
                make(constant: 0),
                make(property: "負"),
             ]),
            (input: "0が正",
             expectedConstants: [(0,"が")],
             expectedInstructions: [
                make(constant: 0),
                make(property: "正"),
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testStringExpressions() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "「monkey」",
             expectedConstants: ["monkey"],
             expectedInstructions: [
                make(constant: 0),
             ]),
            (input: "「mon」と「key」を足す",
             expectedConstants: [("mon","と"), ("key","を")],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
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
                make(op: .opArrayConst, operand: 0),
             ]),
            (input: "配列【1、2、3】",
             expectedConstants: [1, 2, 3],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(constant: 2),
                make(op: .opArrayConst, operand: 3),
             ]),
            (input: "配列【１と２を足す、３から４を引く、５と６を掛ける】",
             expectedConstants: [(1,"と"),(2,"を"),(3,"から"),(4,"を"),(5,"と"),(6,"を")],
             expectedInstructions: [
                make(constant: 0),                      // 1と
                make(constant: 1),                      // 2を
                make(predicate: .ADD),
                make(constant: 2),                      // 3から
                make(constant: 3),                      // 4を
                make(predicate: .SUBSTRACT),
                make(constant: 4),                      // 5と
                make(constant: 5),                      // 6を
                make(predicate: .MULTIPLY),
                make(op: .opArrayConst, operand: 3),
             ]),
            (input: "配列【3個の3】",
             expectedConstants: [3, 3],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(op: .opArrayRepeat),
             ]),
            (input: "nは2。aは「a」。配列【n個のa】",
             expectedConstants: [2, "a"],
             expectedInstructions: [
                make(constant: 0),
                make(op: .opSetGlobal, operand: 0),
                make(constant: 1),
                make(op: .opSetGlobal, operand: 1),
                make(op: .opGetGlobal, operand: 0), // n
                make(op: .opGetGlobal, operand: 1), // a
                make(op: .opArrayRepeat),
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testDictionaryLiterals() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "辞書【】",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opDictionaryConst, operand: 0),
             ]),
            (input: "辞書【１が２、３が４、５が６】",
             expectedConstants: [1,2,3,4,5,6],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(constant: 2),
                make(constant: 3),
                make(constant: 4),
                make(constant: 5),
                make(op: .opDictionaryConst, operand: 6),
             ]),
            (input: "辞書【１が２と３を足す、４が５と６を掛ける】",
             expectedConstants: [1,(2,"と"),(3,"を"), 4,(5,"と"),(6,"を")],
             expectedInstructions: [
                make(constant: 0),  // 1が
                make(constant: 1),  // 2と
                make(constant: 2),  // 3を
                make(predicate: .ADD),
                make(constant: 3),  // 4が
                make(constant: 4),  // 5と
                make(constant: 5),  // 6を
                make(predicate: .MULTIPLY),
                make(op: .opDictionaryConst, operand: 4),
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testRangeLiterals() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "範囲【1以上】",
             expectedConstants: [1],
             expectedInstructions: [
                make(constant: 0),
                make(op: .opComparisonConst, operand: 1),   // 以上
                make(op: .opRangeConst, operand: 2),
             ]),
            (input: "範囲【10以下】",
             expectedConstants: [10],
             expectedInstructions: [
                make(constant: 0),
                make(op: .opComparisonConst, operand: 3),   // 以下
                make(op: .opRangeConst, operand: 2),
             ]),
            (input: "範囲【1以上10未満】",
             expectedConstants: [1,10],
             expectedInstructions: [
                make(constant: 0),
                make(op: .opComparisonConst, operand: 1),   // 以上
                make(constant: 1),
                make(op: .opComparisonConst, operand: 2),   // 未満
                make(op: .opRangeConst, operand: 4),
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testIndexExpressions() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "iは、1と1を足す。配列【１、２、３】のi",
             expectedConstants: [(1,"と"),(1,"を"), 1, 2, 3],
             expectedInstructions: [
                make(constant: 0),  // 1と
                make(constant: 1),  // 1を
                make(predicate: .ADD),
                make(op: .opSetGlobal, operand: 0), // i
                make(constant: 2),  // 1
                make(constant: 3),  // 2
                make(constant: 4),  // 3
                make(op: .opArrayConst, operand: 3),
                make(op: .opGetGlobal, operand: 0), // i
                make(op: .opGenitive),
             ]),
            (input: "配列【１、２、３】の２番目",
             expectedConstants: [1,2,3,2],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(constant: 2),
                make(op: .opArrayConst, operand: 3),
                make(constant: 3),
                make(op: .opGenitive),
             ]),
            (input: "aは、配列【１、２、３】。iは２番目。aのi",
             expectedConstants: [1, 2, 3, 2],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(constant: 2),
                make(op: .opArrayConst, operand: 3),
                make(op: .opSetGlobal, operand: 0),
                make(constant: 3),
                make(op: .opSetGlobal, operand: 1),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 1),
                make(op: .opGenitive),
             ]),
            (input: "iは、2から1を引く。辞書【１が２】のi",
             expectedConstants: [(2,"から"), (1,"を"), 1, 2],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(predicate: .SUBSTRACT),
                make(op: .opSetGlobal, operand: 0),
                make(constant: 2),
                make(constant: 3),
                make(op: .opDictionaryConst, operand: 2),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opGenitive),
             ]),
            (input: "辞書【１が２】の１",
             expectedConstants: [1,2,1],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(op: .opDictionaryConst, operand: 2),
                make(constant: 2),
                make(op: .opGenitive),
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testPhraseExpressions() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "１と２を足す。", // 句は定数
             expectedConstants: [(1, "と"), (2, "を")],
             expectedInstructions: [
                make(constant: 0),                  // 1と
                make(constant: 1),                  // 2を
                make(predicate: .ADD),              // 足す
            ]),
            (input: "aは１。bは２。aとbを足す。",
             expectedConstants: [1, 2],
             expectedInstructions: [
                make(constant: 0),                  // 1
                make(op: .opSetGlobal, operand: 0), // a
                make(constant: 1),                  // 2
                make(op: .opSetGlobal, operand: 1), // b
                make(op: .opGetGlobal, operand: 0), // a
                make(particle: .TO),              // 「と」から「aと」を作る
                make(op: .opGetGlobal, operand: 1), // b
                make(particle: .WO),              // 「を」から「bを」を作る
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
             expectedConstants: [(1, "を")],
             expectedInstructions: [
                make(constant: 0),
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
                make(constant: 0),                  // 1と
                make(constant: 1),                  // 2を
                make(predicate: .ADD),              // 足し
                make(particle: .NI),                // に (直前の「た」は取り除く
                make(constant: 2),                  // 3と
                make(constant: 3),                  // 4を
                make(predicate: .ADD),              // 足し
                make(particle: .WO),                // を (直前の「た」は取り除く
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
                make(constant: 0),                          // 0000 真で
                make(predicate: .BE),                       // 0003 ある
                make(op: .opJumpNotTruthy, operand: 11),    // 0005
                make(constant: 1),                          // 0008 10
                make(constant: 2),                          // 0011 3333
             ]),
            (input: "１が１に等しい場合、【１０】。３３３３。",
             expectedConstants: [(1,"が"), (1,"に"), 10, 3333],
             expectedInstructions: [
                make(constant: 0),                          // 0000 1
                make(constant: 1),                          // 0003 1
                make(predicate: .EQUAL),                    // 0006 等しい
                make(op: .opJumpNotTruthy, operand: 14),    // 0008
                make(constant: 2),                          // 0011 10
                make(constant: 3),                          // 0014 3333
             ]),
            (input: "真である場合、【１０】、それ以外は、【２０】。３３３３。",
             expectedConstants: [(true,"で"), 10, 20, 3333],
             expectedInstructions: [
                make(constant: 0),                          // 0000 真で
                make(predicate: .BE),                       // 0003 ある
                make(op: .opJumpNotTruthy, operand: 14),    // 0005
                make(constant: 1),                          // 0008 10
                make(op: .opJump, operand: 17),             // 0011
                make(constant: 2),                          // 0014 20
                make(constant: 3),                          // 0017 3333
             ]),
            (input: "１が１に等しい場合、【１０】、それ以外は、【２０】。３３３３。",
             expectedConstants: [(1,"が"), (1,"に"), 10, 20, 3333],
             expectedInstructions: [
                make(constant: 0),                          // 0000 1が
                make(constant: 1),                          // 0003 1に
                make(predicate: .EQUAL),                    // 0006
                make(op: .opJumpNotTruthy, operand: 17),    // 0008
                make(constant: 2),                          // 0011 10
                make(op: .opJump, operand: 20),             // 0014
                make(constant: 3),                          // 0017 20
                make(constant: 4),                          // 0020 3333
             ]),
            (input: "１が１である場合、【１０】、それ以外は、【２０】。３３３３。",
             expectedConstants: [(1,"が"), (1,"で"), 10, 20, 3333],
             expectedInstructions: [
                make(constant: 0),                          // 0000 1が
                make(constant: 1),                          // 0003 1で
                make(predicate: .BE),                       // 0006 ある
                make(op: .opJumpNotTruthy, operand: 17),    // 0008
                make(constant: 2),                          // 0011 10
                make(op: .opJump, operand: 20),             // 0014
                make(constant: 3),                          // 0017 20
                make(constant: 4),                          // 0020 3333
             ]),
            (input: "２が１の場合、【１０】、２の場合、【２０】、それ以外は、【３０】。３３３３。",
             expectedConstants: [(2,"が"), (1,"で"), 10, (2,"で"), 20, 30, 3333],
             expectedInstructions: [
                make(constant: 0),                          // 0000 2が
                make(op: .opDuplicateConst, operand: 1),    // 0003 写す
                make(constant: 1),                          // 0005 1で
                make(predicate: .BE),                       // 0008 ある
                make(op: .opJumpNotTruthy, operand: 21),    // 0010 場合
                make(op: .opDropConst, operand: 1),         // 0013 捨てる
                make(constant: 2),                          // 0015 10
                make(op: .opJump, operand: 44),             // 0018 】
                make(op: .opDuplicateConst, operand: 1),    // 0021 写す
                make(constant: 3),                          // 0023 2で
                make(predicate: .BE),                       // 0026 ある
                make(op: .opJumpNotTruthy, operand: 39),    // 0028 場合
                make(op: .opDropConst, operand: 1),         // 0031 捨てる
                make(constant: 4),                          // 0033 20
                make(op: .opJump, operand: 44),             // 0036 】
                make(op: .opDropConst, operand: 1),         // 0039 捨てる
                make(constant: 5),                          // 0041 30
                make(constant: 6),                          // 0044 3333
             ]),
            (input: "真によって、10か20。",
             expectedConstants: [10, 20],
             expectedInstructions: [
                make(op: .opTrue),                          // 0000 真
                make(op: .opJumpNotTruthy, operand: 10),    // 0001 よって
                make(constant: 0),                          // 0004 10
                make(op: .opJump, operand: 13),             // 0007 か
                make(constant: 1),                          // 0010 20
             ]),
            (input: "aは10。bは20。偽によって、aかb。",
             expectedConstants: [10, 20],
             expectedInstructions: [
                make(constant: 0),                          // 0000 10
                make(op: .opSetGlobal, operand: 0),         // 0003 a
                make(constant: 1),                          // 0006 20
                make(op: .opSetGlobal, operand: 1),         // 0009 b
                make(op: .opFalse),                         // 0012 偽
                make(op: .opJumpNotTruthy, operand: 22),    // 0013 よって
                make(op: .opGetGlobal, operand: 0),         // 0016 a
                make(op: .opJump, operand: 25),             // 0019 か
                make(op: .opGetGlobal, operand: 1),         // 0022 b
             ]),
            (input: "bは真。bによって、10か20。",
             expectedConstants: [10, 20],
             expectedInstructions: [
                make(op: .opTrue),                          // 0000 真
                make(op: .opSetGlobal, operand: 0),         // 0001 b
                make(op: .opGetGlobal, operand: 0),         // 0004 b
                make(op: .opJumpNotTruthy, operand: 16),    // 0007 によって
                make(constant: 0),                          // 0010 10
                make(op: .opJump, operand: 19),             // 0013 か
                make(constant: 1),                          // 0016 20
             ]),
            (input: "1が2より大きいかによって、10か20。",
             expectedConstants: [(1,"が"), (2,"より"), 10, 20],
             expectedInstructions: [
                make(constant: 0),                          // 0000 1が
                make(constant: 1),                          // 0003 2より
                make(predicate: .GT),                       // 0006 大きい
                make(op: .opJumpNotTruthy, operand: 17),    // 0008 によって
                make(constant: 2),                          // 0011 10
                make(op: .opJump, operand: 20),             // 0014 か
                make(constant: 3),                          // 0017 20
             ]),
             (input: "aは1。aが1であるかによって、10か20。",
             expectedConstants: [1, (1,"で"), 10, 20],
             expectedInstructions: [
                make(constant: 0),                          // 0000 1
                make(op: .opSetGlobal, operand: 0),         // 0003 a
                make(op: .opGetGlobal, operand: 0),         // 0006 a
                make(particle: .GA),                        // 0009 が
                make(constant: 1),                          // 0011 1で
                make(predicate: .BE),                       // 0014 ある
                make(op: .opJumpNotTruthy, operand: 25),    // 0016 によって
                make(constant: 2),                          // 0019 10
                make(op: .opJump, operand: 28),             // 0022 か
                make(constant: 3),                          // 0025 20
             ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testGenitiveExpressions() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "配列【１、２、３】の1",
             expectedConstants: [1,2,3,1],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(constant: 2),
                make(op: .opArrayConst, operand: 3),
                make(constant: 3),
                make(op: .opGenitive),
            ]),
            (input: "配列【１、２、３】の先頭",
             expectedConstants: [1,2,3],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(constant: 2),
                make(op: .opArrayConst, operand: 3),
                make(property: "先頭"),
            ]),
            (input: "1の負数",
             expectedConstants: [1],
             expectedInstructions: [
                make(constant: 0),
                make(predicate: .NEGATE),
            ]),
            (input: "iは１。iの負数",
             expectedConstants: [1],
             expectedInstructions: [
                make(constant: 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 0),
                make(predicate: .NEGATE),
            ]),
            (input: "1の負数の文字列",
             expectedConstants: [1],
             expectedInstructions: [
                make(constant: 0),          // 1の
                make(predicate: .NEGATE),   // 負数の
                make(property: "文字列"),      // 文字列
            ]),
            (input: "「１」の数値の負数",
             expectedConstants: ["1"],
             expectedInstructions: [
                make(constant: 0),          // 「１」の
                make(property: "数値"),      // 数値の
                make(predicate: .NEGATE),   // 負数
            ]),
            (input: "iは1。配列【１、２、３】のi",
             expectedConstants: [1,1,2,3],
             expectedInstructions: [
                make(constant: 0),
                make(op: .opSetGlobal, operand: 0),
                make(constant: 1),
                make(constant: 2),
                make(constant: 3),
                make(op: .opArrayConst, operand: 3),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opGenitive),
            ]),
            (input: "aは配列【１、２、３】。aの1",
             expectedConstants: [1,2,3,1],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(constant: 2),
                make(op: .opArrayConst, operand: 3),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 0),
                make(constant: 3),
                make(op: .opGenitive),
            ]),
            (input: "aは配列【１、２、３】。iは１。aのi",
             expectedConstants: [1,2,3,1],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(constant: 2),
                make(op: .opArrayConst, operand: 3),
                make(op: .opSetGlobal, operand: 0), // a
                make(constant: 3),
                make(op: .opSetGlobal, operand: 1), // i
                make(op: .opGetGlobal, operand: 0), // aの
                make(op: .opGetGlobal, operand: 1), // i
                make(op: .opGenitive),
            ]),
            (input: "aは配列【１、２、３】。aの最後",
             expectedConstants: [1,2,3],
             expectedInstructions: [
                make(constant: 0),
                make(constant: 1),
                make(constant: 2),
                make(op: .opArrayConst, operand: 3),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 0),
                make(property: "最後"),
            ]),
            (input: "aは1。aが1の場合、1、それ以外は、2。",
             expectedConstants: [1,(1,"で"),1,2],
             expectedInstructions: [
                make(constant: 0),                      // 0000 1
                make(op: .opSetGlobal, operand: 0),     // 0003 a
                make(op: .opGetGlobal, operand: 0),     // 0006 a
                make(particle: .GA),                    // 0009 が
                make(op: .opDuplicateConst, operand: 1),// 0011
                make(constant: 1),                      // 0013 1で
                make(predicate: .BE),                   // 0016 ある
                make(op: .opJumpNotTruthy, operand: 29),// 0018 場合
                make(op: .opDropConst, operand: 1),     // 0021
                make(constant: 2),                      // 0023 1
                make(op: .opJump, operand: 34),         // 0026 それ以外は
                make(op: .opDropConst, operand: 1),     // 0029
                make(constant: 3),                      // 0031 2
            ]),
            (input: "配列【1,2,3】の１と、2を足す。",
             expectedConstants: [1,2,3,1,(2,"を")],
             expectedInstructions: [
                make(constant: 0),                      // 0000 1
                make(constant: 1),                      // 0003 2
                make(constant: 2),                      // 0006 3
                make(op: .opArrayConst, operand: 3),    // 0009
                make(constant: 3),                      // 0012 1
                make(op: .opGenitive),                  // 0015
                make(particle: .TO),                    // 0016 と
                make(constant: 4),                      // 0018 2を
                make(predicate: .ADD),                  // 0021 足す
            ]),
            (input: "aは配列【1,2,3】。aの１とaの2を足す。",
             expectedConstants: [1,2,3,1,2],
             expectedInstructions: [
                make(constant: 0),                      // 0000 1
                make(constant: 1),                      // 0003 2
                make(constant: 2),                      // 0006 3
                make(op: .opArrayConst, operand: 3),    // 0009
                make(op: .opSetGlobal, operand: 0),     // 0012 a
                make(op: .opGetGlobal, operand: 0),     // 0015 aの
                make(constant: 3),                      // 0018 1
                make(op: .opGenitive),                  // 0021
                make(particle: .TO),                    // 0022 と
                make(op: .opGetGlobal, operand: 0),     // 0024 aの
                make(constant: 4),                      // 0027 2
                make(op: .opGenitive),                  // 0030
                make(particle: .WO),                    // 0031 を
                make(predicate: .ADD),                  // 0033 足す
            ]),
        ]
        try runCompilerTests(testPatterns)
    }
    func testGlobalDefStatements() throws {
        let testPatterns: [CompilerTestCase] = [
            (input: "一は１。二は2。",
             expectedConstants: [1, 2],
             expectedInstructions: [
                make(constant: 0),
                make(op: .opSetGlobal, operand: 0),
                make(constant: 1),
                make(op: .opSetGlobal, operand: 1),
             ]),
            (input: "一は１。一。",
             expectedConstants: [1],
             expectedInstructions: [
                make(constant: 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 0),
             ]),
            (input: "一は１。二は一。二。",
             expectedConstants: [1],
             expectedInstructions: [
                make(constant: 0),
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
                (5,"と"),
                (10,"を"),
                [
                    make(constant: 0),
                    make(constant: 1),
                    make(predicate: .ADD),
                    make(op: .opReturnValue),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 2, 0),
             ]),
            (input: "関数【５と１０を足す】",
             expectedConstants: [
                (5,"と"),
                (10,"を"),
                [
                    make(constant: 0),
                    make(constant: 1),
                    make(predicate: .ADD),
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 2, 0),
             ]),
            (input: "関数【１。２】",
             expectedConstants: [
                1,
                2,
                [
                    make(constant: 0),
                    make(constant: 1),
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
                    make(constant: 0),
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
                    make(constant: 0),
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
                    make(constant: 0),
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
                make(constant: 1),
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
                make(constant: 1),
                make(constant: 2),
                make(constant: 3),
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
                make(constant: 1),
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
                make(constant: 1),
                make(constant: 2),
                make(constant: 3),
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
                make(constant: 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opClosure, operand: 1, 0),
             ]),
            (input: "関数【数字は５５。数字】",
             expectedConstants: [
                55,
                [
                    make(constant: 0),
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
                    make(constant: 0),
                    make(op: .opSetLocal, operand: 0),
                    make(constant: 1),
                    make(op: .opSetLocal, operand: 1),
                    make(op: .opGetLocal, operand: 0),
                    make(particle: .TO),
                    make(op: .opGetLocal, operand: 1),
                    make(particle: .WO),
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
             expectedConstants: [([],"に"), (1,"を")],
             expectedInstructions: [
                make(op: .opArrayConst, operand: 0),    // 配列【】
                make(property: "数"),                    // 数
                make(constant: 0),                      // 配列【】に
                make(constant: 1),                      // 1を
                make(predicate: .APPEND),               // 追加
             ]),
            (input: "配列【】。数。配列【】。１を追加",
             expectedConstants: [(1,"を")],
             expectedInstructions: [
                make(op: .opArrayConst, operand: 0),    // 配列【】。
                make(property: "数"),                    // 数
                make(op: .opArrayConst, operand: 0),    // 配列【】。
                make(constant: 0),                      // 1を
                make(predicate: .APPEND),               // 追加
             ]),
            (input: "関数【配列【】の数】",
             expectedConstants: [
                [
                    make(op: .opArrayConst, operand: 0), // 配列【】。
                    make(property: "数"),                // 数
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(op: .opClosure, operand: 0, 0),    // 関数
             ]),
            (input: "関数【配列【】。数】",
             expectedConstants: [
                [
                    make(op: .opArrayConst, operand: 0),// 配列【】。
                    make(property: "数"),                // 数
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
                    make(particle: .TO),                // と
                    make(op: .opGetLocal, operand: 0),  // b
                    make(particle: .WO),                // を
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
                    make(particle: .TO),                // と
                    make(op: .opGetFree, operand: 1),   // b
                    make(particle: .TO),                // と
                    make(op: .opGetLocal, operand: 0),  // c
                    make(particle: .WO),                // を
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
                    make(constant: 3),  // 88
                    make(op: .opSetLocal, operand: 0),  // c
                    make(op: .opGetGlobal, operand: 0), // global(55)
                    make(particle: .TO),                // と
                    make(op: .opGetFree, operand: 0),   // a
                    make(particle: .TO),                // と
                    make(op: .opGetFree, operand: 1),   // b
                    make(particle: .TO),                // と
                    make(op: .opGetLocal, operand: 0),  // c
                    make(particle: .WO),                // を
                    make(predicate: .ADD),              // 足す
                    make(op: .opReturn),
                ],
                [
                    make(constant: 2),  // 77
                    make(op: .opSetLocal, operand: 0),  // b
                    make(op: .opGetFree, operand: 0),   // a
                    make(op: .opGetLocal, operand: 0),  // b
                    make(op: .opClosure, operand: 4, 2),// 関数【cは、88。...
                    make(op: .opReturn),
                ],
                [
                    make(constant: 1),  // 66
                    make(op: .opSetLocal, operand: 0),  // a
                    make(op: .opGetLocal, operand: 0),  // a
                    make(op: .opClosure, operand: 5, 1),// 関数【bは、77。...
                    make(op: .opReturn),
                ],
             ],
             expectedInstructions: [
                make(constant: 0),      // 55
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
                    make(particle: .KARA),
                    make(constant: 0),
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
                make(constant: 2),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opCall),
             ]
            ),
            (input: "wrapperは、関数【countDownは、関数【入力がx。xから1を引き、countDownする】。1をcountDownする】。wrapperを実行。",
             expectedConstants: [
                (1,"を"),
                [
                    make(op: .opGetLocal, operand: 0),
                    make(particle: .KARA),
                    make(constant: 0),
                    make(predicate: .SUBSTRACT),
                    make(op: .opCurrentClosure),
                    make(op: .opCall),
                    make(op: .opReturn),
                ],
                (1,"を"),
                [
                    make(op: .opClosure, operand: 1, 0),
                    make(op: .opSetLocal, operand: 0),
                    make(constant: 2),
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
    func testStackOperations() throws {
        let tests: [CompilerTestCase] = [
            (input: "捨てる。",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opDropConst, operand: 1),
             ]
            ),
            (input: "2個捨てる。",
             expectedConstants: [],
             expectedInstructions: [
                make(op: .opDropConst, operand: 2),
             ]
            ),
            (input: "nは3。n個捨てる。",
             expectedConstants: [3],
             expectedInstructions: [
                make(constant: 0),
                make(op: .opSetGlobal, operand: 0),
                make(op: .opGetGlobal, operand: 0),
                make(op: .opDrop),
             ]
            ),
        ]
        try runCompilerTests(tests)
    }
    // MARK: - Helpers
    private func runCompilerTests(_ tests: [CompilerTestCase], isOptimized: Bool = false) throws {
        for t in tests {
            print("テスト開始：「\(t.input)」")
            let program = parseProgram(with: t.input)!
            let compiler = Compiler(from: program)
            compiler.optimizeConstantsEnabled = isOptimized
            if isOptimized {
                switch compiler.analyze() {
                case .constant(let analyzed):
                    print("テスト終了：\(analyzed.string)")
                    continue
                default:
                    break
                }
            }
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
            case let token as Token:
                let particleIndex = token.particleIndex
                XCTAssertEqual(particleIndex, actual.number)
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
