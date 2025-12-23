//
//  frame.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/10/12.
//

import Foundation

/// 実行フレーム(アクティベーションレコード)
class Frame {
    init() {}
    init(with cl: JpfClosure, basePointer: Int) {
        self.cl = cl
        self.basePointer = basePointer
        self.sp = basePointer + cl.fn!.numberOfLocals
    }
    var cl: JpfClosure?
    var ip = -1             // フレーム内の実行位置
    var basePointer = 0     // ローカル変数のスロット位置
    var sp = 0              // Frameのsp初期値
    //
    var insturctions: Instructions? {cl?.fn?.instructions}
    // ip制御
    var isIpInRange: Bool {
        guard let i = insturctions else {return false}
        return ip < i.count - 1
    }
    func advanceIp(by offset: Int) {ip += offset}
    func advanceIp() -> Int {
        ip += 1
        return ip
    }
    func setIp(to position: Int) {ip = position}
}
