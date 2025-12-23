//
//  errors.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2025/11/24.
//
// MARK: - メッセージ(使い方)
let printUsage =            JpfError("仕様：(〜と…)〜を表示する。")
let askUsage =              JpfError("仕様：(〜と…)〜と尋ねる。")
let readUsage =             JpfError("仕様：(〜と…)〜を音読する。")
let additionUsage =         JpfError("仕様：(〜と…)〜を足す。")
let multiplicationUsage =   JpfError("仕様：(〜と…)〜を掛ける。")
let substractionUsage =     JpfError("仕様：(〜から)〜を引く。または、(〜を)〜から引く。")
let divisionUsage =         JpfError("仕様：(〜を)〜で割る。または、(〜で)〜を割る。")
let negateUsage =           JpfError("仕様：〜の負数。または、〜を負数にする。")
let equalUsage =            JpfError("仕様：〜(と)〜(が)等しい。")
let beUsage =               JpfError("仕様：(〜が)〜である。または、(〜は)〜である。")
let notUsage =              JpfError("仕様：(〜が)〜で(は)ない。または、(〜は)〜で(は)ない。")
let returnValueUsage =      JpfError("仕様：(〜を)返す。")
let appendUsage =           JpfError("仕様：〜(を)〜に追加する。または、〜(に)〜を追加する。")
let appendDictionaryUsage = JpfError("仕様：〜が〜(を)〜に追加する。または、〜(に)〜が〜を追加する。")
let removeUsage =           JpfError("仕様：(〜から)〜を削除する。")
let rangeCheckUsage =       JpfError("仕様：<数値>が範囲【<範囲式>】に")
let determineUsage =        JpfError("仕様：〜が<配列、範囲>に")
let containsUsage =         JpfError("仕様：<配列、辞書、範囲>が<値/要素>を含む。")
let foreachUsage =          JpfError("仕様：<配列、辞書、範囲>で<関数>を繰り返す。")
let mapUsage =              JpfError("仕様：<配列、辞書、範囲>を<関数>で写像する。または、<範囲>を写像する。")
let filterUsage =           JpfError("仕様：<配列、辞書>を<関数>で絞り込む。")
let reduceUsage =           JpfError("仕様：<配列、辞書、範囲>を<初期値>と<関数>でまとめる。")
let sortUsage =             JpfError("仕様：<配列>を<関数>で並べ替える。または、<配列>を(「昇順」/「降順」に）並べ替える。")
let reverseUsage =          JpfError("仕様：<配列、文字列>を逆順にする。")
let pullDupUsage =          JpfError("仕様：(「<識別子>」と…)(「<識別子>」に）(「数値」または「値」を)(<数値>個)")
let assignUsage =           JpfError("仕様：〜(を)「<識別子>」に代入する。または、「<識別子>」に〜を代入する。")
let compoundAssignUsage =   JpfError("仕様：<識別子>(に)<計算し>て代入する。")
let assignArrayUsage =      JpfError("仕様：〜(を)<配列>の位置<数値>に代入する。または、<配列>の位置<数値>に〜を代入する。")
let swapUsage =             JpfError("仕様：<識別子１>と<識別子２>を入れ替える。または、<識別子１>を<識別子２>と入れ替える。")
let createUsage =           JpfError("仕様：(「<識別子>」を)(<引数>で)<型>から生成する。または、<型>から(<引数>で)「<識別子>」を生成する。")
let createEnumeratorUsage = JpfError("仕様：(「<識別子>」を)<値>で<列挙型>から生成する。または、<列挙型>から<値>で「<識別子>」を生成する。")
let setUsage =              JpfError("仕様：<値>(を)<オブジェクト>の要素「<識別子>」に設定する。または、<オブジェクト>の要素「<識別子>」に<値>を設定する。")
// MARK: - 共通エラー
func undefinedIdentifier(_ ident: String) -> JpfError {
    JpfError("識別子『\(ident)』が定義されていない。")
}
var notEnoughStackValues: JpfError      {JpfError("スタックの値の数が不足している。")}
// MARK: - ヘルパー
@discardableResult
func jpfError(from err: Error) -> JpfError {
    guard let jpfError = err as? JpfError else {
        assertionFailure("JpfError以外のエラーは想定していない。")
        fatalError("Unexpected non-JpfError thrown")
    }
    return jpfError
}

