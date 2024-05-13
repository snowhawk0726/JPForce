# 日本語ふぉーす(JpForce)

- [言語の説明](#言語の説明)
- [文法・言語仕様](./ProgrammingLanguageGuide.md)(別ドキュメント)
- [コマンド(REPL)](#インタープリタjpforce-コマンド)

## 言語の説明

### 目標

日本語ふぉーす(JpForce)は、コンピュータのプログラムを、日本語のコメントや仕様書の様に読み書きできることを目標としたプログラミング言語です。

一般的なプログラミング言語は、プログラムを英語や記号で記述しますが、プログラムを理解しやすい様に日本語のコメントによって定義等の意味や仕様を補うことが多く、それが逆に可読性を下げたり、保守性を悪くすることがあると思います。  
英語や記号に対するコメントを避け、可読性、保守性を上げるためには、日本語に近いプログラミング言語で、理解しやすいプログラムを書ける様にすることが有効と考えます。

目標を達成するために、以下の様な機構、仕組みを取り入れています。

また、このような機構はプログラミング言語 Forth で採用されています。Forth は軽量で移植性が高い言語で、これにあやかり、本言語を「日本語ふぉーす」（以下「ふぉーす」）と命名しました。  
英語名の Force は、誤字でなく、「力」もしくは「無理矢理」というニュアンスです。

### 機構

#### 逆ポーランド記法

逆ポーランド記法とは、数式やプログラム記法の一種で、一般的なプログラミング言語とは異なり、演算子を被演算子の後方に記述します。
例えば、一般的なプログラミング言語の記法では、

```swift
print(3 + 4)
```

の様な順に記述しますが、ふぉーすでは、

```
3と4を足し、表示する。
```

の様に、逆ポーランド記法で記述することにより、日本語の語順でプログラムを表現できます。

#### スタック

プログラムを実行する際、定義された識別子や引数を、一旦「スタック」というメモリ領域に格納し、述語で(格納した順とは逆順に)取り出し、演算等を行います。

_例：_

```
4から1を引く。
```

上記では、４、１の順にスタックに格納し、`引く`で１、４の順に取り出し演算を行います。

ただし、ふぉーすでは関数等で定義した順に、識別子にスタックの引数を割り当てる機構があるため、スタックの順を意識してプログラミングを行う必要はありません。

#### 助詞

日本語は、述語(動詞、形容詞、名詞）を中心に構成され、述語と他の成分は、格助詞「を、に、まで、が、より、から、で、へ、と」で関係づけられています。  
また、助詞「は」は、主題とその説明を関係付け、格助詞「の」は、名詞と名詞をつなぐ働きがあり、つながれた名詞全体で 1 つの成分となります。

ふぉーすでは、これらの助詞や格助詞を区切り文字として識別し、文中の成分(単語)を識別します。  
一般的なプログラミング言語では、助詞ではなく、スペース(空白)や記号を区切り文字としていますが、これを採用すると、日本語として違和感があるため、この様な機構を採用しています。

#### 定義

一般的な言語では、変数、関数、クラスを表すキーワードを付けた識別子に対して定義します。

```swift
var a = 1       // 変数 a に初期値 1 を定義
func f() {…}    // 関数 f を定義
class c {…}     // クラス c を定義
```

ふぉーすでは、助詞`は`を使って次の様に定義します。

```
aは、1
fは、関数であって、…
cは、型であって、…
```

助詞`は`によって、主題(識別子)に対応する説明(定義)を関連付けるという、日本語の構造に沿った定義法になります。

#### 連用形

プログラムは、複数の述語(命令）を並べて順次実行することがありますが、一連の動作などは、述語が１つである単文を並べるのでなく、複文で記述する方が読みやすくすることが可能です。  
日本語で複文を記述する場合、文末以外の述語は、語尾が連用形に変化します。例えば、

_例：_

```
aをbで割り、cを掛ける。
```

この例では、終止形「割る」が連用形の「割り」と変化しています。
ふぉーすでは、「連用形」→「終止形」の辞書を持っており、「割り」を「割る」と同じものとして処理することにより、日本語的な表記を可能としています。

#### 記号

一般的なプログラム言語は、数式をベースにしており、簡潔さのため、記号を多様する傾向がある様に思います。

_例：_

```swift
let b = a.x ? c : d   // a の x が true なら c、false なら d、を b に代入する。
```

これは、数式に関するプログラミングには適しており、慣れれば分かりやすいですが、多用すると理解する上で問題になったり、あとで分からなくなったりする原因となる場合があります。  
ふぉーすでは、記号の多用を避け、日本語文に用いる記号を採用し、読み易さ(聞き易さ)と、理解し易さを優先しています。

以下は、上記をふぉーすで表記した場合の例です。

_例：_

```
bは、aのxによって、cかd。
(もしくは)
bは、aのxが真であるかによって、cかd。
```

記号に頼らず、文章的にすることより、プログラミングを音声により読み上げても、意味を把握しやすくなっています。

### 表記・表現方法

ふぉーすは、上記の様にプログラムを日本語のコメントの様に読み書きできるようにすることを目標としていますが、同時にプログラミング言語としては、一般的なプログラミング言語と同等の機能、表現を可能とすることを目指しています。

これは、一般的なプログラミング言語で用いられる定義方法やパターンなどを取り入れることができる様にし、他プログラミング言語の参考書やサンプルを日本語訳する様に記述することで、採用可能とするためです。

例えば、一般的なプログラミング言語 Ruby で、

```Ruby
class Duck
    def sound
　      'quack'
    end
end
```

は、ふぉーすでは、

```
アヒルは、型であって、鳴くは関数で【本体が、「ガー」】。
```

の様に、プログラムの日本語コメントを書く様に記述することができます。
これを実現するために、以下の様な表現方法を採用しています。

#### キーワード

プログラミングで使用するキーワード(予約語)は、なるべく独自でない、言語のマニュアルや参考書で使われる様な日本語訳を採用しています。例えば、以下の通り。

- string → 文字列
- func → 関数
- true → 真
- protocol → 規約
- range → 範囲

#### 冗長表現

読みやすさを考慮し、表記に冗長性を持たせています。

_例:_

```
甲は、関数であって、入力が〜
甲は、関数であり、入力が〜
甲は、関数【入力が〜】
```

これにより、文章の様に書いたり、簡潔なプログラムとして書いたり、メリハリをつけることができ、理解しやすく工夫することが可能と考えます。

#### 構造表現

制御文や型定義等でプログラムが入れ子になる、もしくは複数の文からなる場合、ふぉーすでは、隅付き括弧`【】`で構造を表します。(括弧内をブロックと呼ぶ)  
ブロックが一行で収まる場合は、`【】`を省略することができます。また、ブロック内のインデント、空白、改行等は読み飛ばします。

例えば、1 から 12 までのフィボナッチ数を表示する場合、以下の様に記述します。

```
フィボナッチ数は、関数であって、【入力が数字「数値」であり、本体が、
    数字が、
    １の場合、1を返す。
    ２の場合、2を返す。
    それ以外は、
        数字から1を引き、フィボナッチ数を実行したものと、
        数字から2を引き、フィボナッチ数を実行したものを、足す。
】。
1から12まで反復【入力が数、数でフィボナッチ数を実行し、表示する】。
```

このように必要に応じてブロックで表記し、それ以外は省略することができます。

### 仕様

仕様・言語の使い方については、[「文法」](./ProgrammingLanguageGuide.md)に記載しています。

### 今後の見通し

ご意見を受けたり、プログラムを書いて、言語仕様にフィードバックをかけられたら思っています。  
また、機能的には、特に拡張性(述語(組み込み関数)に対する多重定義、型(組み込み型)の拡張、辞書の強化(永続化、モジュール化等))や、強化(エラー処理、スレッド)ができれば良いと思います。  
ただし、別途、コンパイラ版も作りたいので、技術を習得できたら、こちらからやりたいです。

コンパイラ版は、以下の様にできれば良いと考えます。

- インタープリタ(REPL)は継続。(デバッグ機能を搭載)
- 定義文を評価時にコンパイルし、辞書に登録する。
- 実行時は、上記の実行オブジェクトを VM で動作させる。
- 辞書は、実行環境と共にモジュール(ライブラリ)として、可搬(ファイル)化できる様にする。
- 組み込みの述語(予約語)は、極力モジュールとする。
- 最終的には、REPL、コンパイラ共にふぉーす自身で記述する。

妄想はできますが、私の実力では 10 年とかかかりそうです。

### 参考文献

- [Go 言語でつくるインタプリタ](https://www.oreilly.co.jp/books/9784873118222/)

  - Thorsten Ball 　著、設樂 洋爾　訳
  - オライリー・ジャパン (オーム社)
  - ISBN 978-4-87311-822-2

  JPForce コマンドは、この本の内容がベースになっています。この本が無ければここまでできていません。

- [日本人のための日本語文法入門](https://bookclub.kodansha.co.jp/product?item=0000210659)

  - 著：原沢　伊都夫
  - 講談社現代新書
  - ISBN 978-4-06-288173-9

  特に助詞の使い方・意味を参考にしています。

- [Swift 実践入門](https://gihyo.jp/book/2020/978-4-297-11213-4)

  - 石川洋資，西山勇世　著
  - 技術評論社
  - ISBN 978-4-297-11213-4

  Swift は、開発言語であり、開発はもちろん、ふぉーす文法の参考にしています。

- [IT 用語辞典 e-Words](https://e-words.jp/)  
  コンピュータ用語や、対応する英語などの参考にしています。

## インタープリタ(JPForce コマンド)

言語の仕様を検証するために、Swift で作成した CLI インタープリタ`JPForce`を用意しています。

### ビルド

リポジトリーをコピーし、xcode でコンパイルします。

### 開発/実行環境(2024 年 5 月 現在)

- Swift: Swift 5
- xcode: Version 15.3 (15E204a)
- MacOS: 14.4.1（23E224）

### 実行

#### REPL の起動

ビルドされた`JPForce`コマンドを実行することにより、以下の様に REPL(Read-Eval-Print Loop)が起動し、プロンプト`>>`が表示されます。

_ターミナルのイメージ_

```
% ./JPForce
日本語ふぉーす(JPForce)のREPLです。
>>
```

ここに、プログラムを記述し改行することにより、一行ごとにプログラムが実行されます。

複数行のプログラムを実行するには、まず、ファイルにテキストでプログラムを記述し、「書類」フォルダーに保存します。  
次に、プロンプトに`ファイル「<ファイル名>」を実行する。`を入力することにより、プログラムが実行されます。

REPL を終了するには、`Control + D`を入力します。

注：xcode 上で、REPL を起動した場合、プロンプトの後に日本語を直接入力することはできません。`Control + F`で、検索を開いて、そこに入力した日本語をコピペするか、メモ帳等で書いてコピペする必要があります。

[「文法」](./ProgrammingLanguageGuide.md)に記載している例も、基本的にコピペで、実行できる様にしています。  
(スタックは、誤動作の原因にもなるので、不要であれば、空にしてから実行することをお勧めします。)

#### REPL の実行結果の見方

プログラムは、インタープリタにより、まず、字句解析器(Lexer)によって、トークン(Token)に分解します。  
その後、構文解析器(Parser)によって、構文木(AST: Abstract Syntax Tree)を構成します。  
そして、評価器(Evaluator)によって、構文木を辿ることにより、評価・実行されます。

##### 表示色

関数・算出・型・規約などは、定義文で評価される際に、一部(本体部)が、構文木として辞書に登録されます。  
このため、述語`表示`によって、識別子を表示すると構文木を再構築されたものが、文字列として表示され、同時にトークンの属性によって、色分けされます。(トークンの色分けについては、[解析結果の見方](#解析結果の見方)を参照。)

注１：`関数`、`数値`などのオブジェクトの型および、`真`、`偽`、`無`などのリテラルは、マゼンタで色表示します。  
注２：表示色は、Release バージョンのみです。(xcode 上でも表示されません。表示色はエスケープ文字で実現しており、xcode で Release バージョンを起動した場合、色の代わりに`[31m`等のエスケープ文字が表示されます。)

##### 評価結果

プログラムを実行した結果、すなわち、プログラムが最終的に返した値を`評価結果：`として表示します。ただし、定義文等、値を返さない場合は、`評価結果`は表示されません。

##### エラー表示

字句解析時にエラーを検出した場合は、以下の様な形式でエラーを表示します。

```
形式：字句解析エラー： <エラーの内容>。
```

例えば、文字列の閉じ括弧`」`を入力し忘れた場合、以下の様に表示されます。

```
>> 関数であって、【「何もしない】
字句解析エラー： 」が見つからない。
構文解析器が、4個のエラーを検出した。
	式の解析で、「ILLEGAL("」が見つからない")」に対応する構文解析方法が実装されていない。
	式文で、式の解析に失敗した。
	ブロック(【】)内で文の解析に失敗した。
	式文で、式の解析に失敗した。
>>
```

注： 字句解析器は、トークンにエラーを出力するので、それを受けて構文解析器もエラーを表示します。

また、構文解析時にエラーを検出した場合は、以下の様な形式でエラーを表示します。

```
形式：構文解析器が、<n>個のエラーを検出した。
　　　<各エラーの詳細>(解析位置：<現トークンの文字>)
```

例えば、範囲の指定を誤った場合、以下の様に表示されます。

```
日本語ふぉーす(JPForce)のREPLです。
>> 甲は、範囲【１から２以上】。
構文解析器が、3個のエラーを検出した。
	範囲で、範囲式の解析に失敗した。(下限「から」が重複)(解析位置: 】)
	式文で、式の解析に失敗した。(解析位置: 】)
	定義文「<識別子>は、<式(値)>。」で、式の解釈に失敗した。(解析位置: 】)
>>
```

`解析位置`とは、解析エラーが発生した際のトークンを示しています。上記の例では、`】`の前まで、解析しエラーを検出したことを意味します。

構文エラーは、構文解析器が AST を辿っているため、エラーが検出箇所から、木の上方へと伝搬します。  
上記例では、`範囲`の解析でエラーを検出し、その上位の式文`範囲【１から２以上】`、さらに、定義文`甲は、範囲【１から２以上】。`の解析のエラーが伝わっています。

注：「式文」とは、複数の式からなる文のことです。

##### スタック表示

プログラムがスタックに格納した値を、`入力： (<スタックの値>)`として表示します。スタックは、古い順にスペースで区切られ表示されます。

スタックは、プログラム終了まで、自動的に削除されることはありません。  
スタック内容が不要である場合は、述語`空`で削除してください。  
(`空`と入力するより、`空にする`と入力した方が変換が早いです。)

```
日本語ふぉーす(JPForce)のREPLです。
>> 1と2を
評価結果: 2を
入力: (1と 2を)
>> 空にする
入力: ()
>>
```

注：評価結果、およびスタックに表示される文字列(STRING)には、かぎ括弧`「」`が付きません。ただし、`関数`等の本体、すなわち、AST から再構築された文字列には、かぎ括弧`「」`が表示されます。

![イメージ画像](./JPForceStack.png)

#### サンプルプログラム

本ドキュメントや文法書にあるサンプル等をまとめたファイル「[サンプル.txt](./サンプル.txt)」を用意しています。

書類フォルダーにファイルを置き、サンプル.txt の一行目にあるとおり、`ファイル「サンプル.txt」を実行`し、コンソールにコメント記号`※`のある行のプログラムをコピペすることにより、動作が確認できます。

#### 字句解析器(Lexer)

REPL のプロンプト`>>`で`改行`することにより、字句解析器(Lexer)が起動します。

```
日本語ふぉーす(JPForce)のREPLです。
>>
字句解析器(Lexer)動作確認。
テストする日本語を入れてください：

```

ここに、プログラムや文章を入力することにより、解析結果が表示されます。

字句解析器を終了するには、`Control + D`を入力します。

注：コマンドが終了します。REPL には戻りません。

##### 解析結果の見方

入力した文字列の下に`tagged:`と書かれた行が表示されます。  
これは、字句解析器が文字列をトークンに分解する前に、文字列を語`(w)`と記号`(s)`に分解し、それぞれにタグ付けしたものです。`tagged:`の後ろは、分解・タグ付けされた文字列を表示しています。

次に、`tokenized:`と書かれた行が続きます。  
これは、トークンに分解された文字列と、その属性を表しています。また、トークンは属性によって、色分けされています。

![色分けの例](./JPForceLexer.png)

トークンの属性(TokenType)は、以下の通りです。

1. `IDENT` (シアン)：
   識別子。
1. `INT` (青)：
   数値。
1. `STRING` (赤)：
   文字列。
1. `symbol` (無し)：
   記号。
1. `keyword` (緑)：
   予約語。
1. `particle` (マゼンタ)：
   助詞。
1. `wrapped` (色は属性による)：
   識別子、予約語、または助詞の連用形。

注：予約語、助詞、記号の括弧内は、token.swift 内の列挙子です。

### 課題

実力不足により、多々課題はありますが、特に以下がうまくいっていません。

- ブロック`【】`と EOL によるブロック終了処理(Parser)
  設計があいまいなため、改行等でブロックがうまく解析できなくなります。制約(ルール)を設け、テスト可能な形で作り直したいですが、現在まで、都度対応しています。
- エラー検出
  プログラム上、エラーになる場合にメッセージを表示しているだけなので、よくわからなく対処に困るメッセージになっていると思います。
- 字句解析
  テキストからトークンを切り出す前段として、NLTagger を使用しています。  
  Word と Symbol に分解し、Tag 付けを行っていますが、Word が細かすぎ(例：「または」を「また」「は」と分解)、~~また、絵文字が消えてしまうような、副作用があります。~~(現在は確認できない)  
  排除して、自前で分解した方が良いのでは無いかと考えていますが、手付かずです。
- 連用形辞書
  辞書をメンテナンスする述語等を設けていません。辞書にない語尾をもつ語を使用する場合は、token.swift の class ContinuativeForm の辞書を修正する必要があります。

### テスト

xcode の XCTest (UnitTests) で、Token, AST, Object, Lexer, Parser, Evaluator の単体テストをしています。

## ライセンス

[MIT](./LICENSE)
