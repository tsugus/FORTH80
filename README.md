# FORTH80　 Ver. 0.8.3

- [FORTH80　 Ver. 0.8.3](#forth80-ver-083)
    - [Abstract](#abstract)
  - [これは何？](#これは何)
  - [使い方](#使い方)
  - [バージョン番号について](#バージョン番号について)
  - [セルフホスティング](#セルフホスティング)
  - [起動オプション](#起動オプション)
    - [-h](#-h)
    - [-s](#-s)
    - [-u](#-u)
    - [-v](#-v)
  - [プログラミング言語 FORTH の初歩の初歩](#プログラミング言語-forth-の初歩の初歩)
    - [スタックと計算](#スタックと計算)
    - [コロン定義](#コロン定義)
    - [制御構造](#制御構造)
    - [変数および配列](#変数および配列)
    - [ボキャブラリ](#ボキャブラリ)
    - [倍長整数と小数点「もどき」](#倍長整数と小数点もどき)
    - [文字列](#文字列)
    - [外部記憶装置](#外部記憶装置)
  - [入出力について](#入出力について)
  - [独自の FORTH ワード](#独自の-forth-ワード)
    - [PFLAG](#pflag)
    - [UTF-8](#utf-8)
    - [ECHO](#echo)
    - [UVR](#uvr)
    - [SCR/DRIVE](#scrdrive)
    - [POPEN](#popen)
      - [使い方の例](#使い方の例)
    - ["r"](#r)
    - ["w"](#w)
    - ["r+"](#r-1)
    - [PCLOSE](#pclose)
    - [SET-INPUT](#set-input)
    - [SET-OUTPUT](#set-output)
  - [定義しておくと便利なワード](#定義しておくと便利なワード)
    - [{, }](#-)
    - [.\\](#)
    - [HOME, CLS](#home-cls)

### Abstract

An emulator of the programming language FORTH.

FORTH80 compliants with FORTH-79 standard.

----
## これは何？

FORTH というプログラミング言語の処理系の、一種のエミュレータです。
あえて古い形式のものをエミュレートしています。

FORTH-79 Standard に準拠しています（ただし、現時点で徹底したチェックはしておりません）。

これは処理系そのものではなく、実質、エミュレータであることに注意してください。
しかしながら、ターミナルを操作する機能を付加したことにより、まったく孤立した「箱庭」ではなくなったので、スクリプト言語のように使うことも不可能ではないかもしれません。

井上外志雄『標準 FORTH』(共立出版，1985）で解説されている FORTH 言語の仕様に従っています。
それゆえ、この書籍を読む際には少なからず役立つと思います。

## 使い方

以下のファイルを同一フォルダ内に置いてください。
専用のフォルダを用意することをお奨めします。

- VSM, VSM.exe のどちらか一方
- FORTH80.bin
- DRIVE0.img
- DRIVE1.img

VSM.exe は Windows 用、VSM は Mac 用です。
Mac 版のソースコードを再コンパイルすれば（ソースファイル VSM_mac.c。ただし同梱の conio.h が必要） Linux でも動くと思います。

Windows の場合は、VSM.exe をダブルクリックすればコンソール画面が立ち上がります。
Mac の場合は、ターミナルから ./VSM と入力して起動しないとダメです。

それ以前に、「このアプリは安全が保証されていないので実行中止」の趣旨のダイアログ、もしくはメッセージが出るかもしれません。
そのときは、Windows の場合は、ダイアログの「詳細」のところをクリックして出てくる「実行」ボタンをクリックしてください。
Mac の場合は、Control キーを押しながら VSM をクリックして「開く」を選択、すると「本当に開いていいか」という趣旨のダイアログが出るので、「開く」をクリックしてください。
Mac だとそれでもファイルの実行権限まわりのセーフティ機能が働くのか、VSM が FORTH80.bin を開けずにすぐ終了してしまいます。
ですが、ターミナルから ./VSM と入力して開くと正常稼働します。
いずれの場合も、以降は OS によって実行禁止されることはありません。

さて、FORTH80 を起動してもプロンプトは表示されません。
しかし、入力すれば反応するはずです（正しく起動できていればそのはず）。
最初のバージョン表示以外、何も表示されていないことに驚かないでください。
すでに FORTH インタプリタは起動しています。

FORTH80 を**終了するには FORTH ワード「BYE」を入力**します。

> BYE&crarr;

で元のターミナルやコンソールの画面に戻ります。
ダブルクリックで起動した場合（Windows）はウィンドウが閉じます。

***注意：使用は自己責任で！***

***CAUTION: Use at your own risk!***

## バージョン番号について

ややこしいですが、FORTH80 にはシステム全体としては三つの構成要素があり、それぞれ別にバージョン番号が振ってあります。すなわち、バージョン番号は三種類あります。

1. FORTH80 のシステム的中核部、すなわち、コンピュータに例えれば OS に当たるものバージョン。FORTH80.asm（MASM 用）および FORTH80.fth（セルフホスティング用）。ver. x.x.x などと記す。
2. FORTH80 のバーチャルマシンのバージョン。VSM_mac.c および VSM_win.c。VM v. x.x.x などと記す。
3. ユーティリティが書きこまれた仮想ディスク・イメージのバージョン。DRIVE0.img および DRIVE1.img。data v. x.x.x などと記すが、DRIVE0.img の中ではふつうに Ver. ... と記している。

項目 2 と 3 は、項目 1 のサブカテゴリーのようなもので、それらのバージョン番号は項目 1 のそれと必ずしも一致しません。

## セルフホスティング

バージョン 0.7 においてセルフホスティングを達成しました。
ただし、厳密な意味でのそれにはなっていないかもしれません。
インタプリタへの直接入力に処理が依存している部分がわずかながらあるからです。
それを除けば、ともかくもセルフホスティングのようなことは行えています。

つまり、FORTH80.bin のソースコードを FORTH で書くことができました。
（VSM は仮想マシンでありハードウェアの範疇に入るのでセルフホスティングの対象外です。あしからず。）

（以前はこの文章において、「この形式のセルフホスティングはあまり役に立たない」と書いていました。
それは「システムの基本的なパラメータを書き換えることに使えない」という理由でしたが、それは私の誤解だったということに気づいたので、削除訂正しておきます。）

さて、具体的な「セルフホスティング」の手順は以下のようになります。
FORTH80.fth（現在のところ、リポジトリの src フォルダにあります）がそのための中心的な FORTH プログラムコードです。
ただし、実は FORTH エディタ用のコマンドが各行頭に追加されています。
行番号のように見える「\<n\> P」というのがそれで、これは、エディタのコマンドにおける「\<n\> 行目に P の後ろのテキストを入力せよ」を意味します。
そのエディタのプログラムは（仮想）ドライブ 0 のスクリーン 6 をロードすると読み込まれるようになっています。
つまり、まずエディタを有効にして、なおかつドライブ 1 を指定し、FORTH80.fth の内容を一括してコピペすれば、自動でドライブ 1 の特定の領域（「\<n\> LIST」でスクリーン \<n\> が指定される）に一連の内容が書き込まれます。

<s>しかし逆に言えば、いくら自動とはいえ、最初から DRIVE1.img に書き込んでおけばこのような手間は必要ないのですが、そこはソースコードの管理上、そうしたくない理由がこちらにあるのです。
再びご了承ください。</s>
（ソースコードを書き込んだ DRIVE1.img を、リポジトリの exe フォルダ内に用意しました。したがって、同フォルダ内の VSM もしくは VSM.exe を起動した場合、上記の書き込み手順および下記の手順 2. から 6. は必要ありません。）

ともかくも、

1. FORTH80 を起動。
2. 「DR0&crarr;」でドライブ 0 を選択。
3. 「6 LOAD&crarr;」で FORTH エディタをロード（その時点でエディタは有効）。
4. 「DR1&crarr;」でドライブ 1 を選択（上書きしてよいものと前提）。
5. FORTH80.fth の中身をコピペしてドライブ 1 に自動書き込み。
6. 「BYE&crarr;」で FORTH から出てターミナルに戻る。
7. 「./VSM -u < start.txt&crarr;」で FORTH80 を起動してセルフホスティングを実行。自動で FORTH80.bin を上書き。（start.txt はリポジトリの src フォルダにあります。したがって、リポジトリをそのままコピーし、かつ、exe フォルダをカレントディレクトリにしているならば、「./VSM -u < ../src/start.txt&crarr;」と入力することになります。）

で、セルフホスティング的なことが行えます。
補助的に star.txt という一種のパッチファイルが必要になります。
大部分はソースコードを仮想ドライブ内に書き込む手順で、実質的なセルフホスティングのための操作は手順 7 だけです。

ちなみに、最初の FORTH80.bin はアセンブリ版からのコンバイルですので、セルフホスティングで得られるものとバイナリの内容が微妙に違います。
しかし、もう一度、手順 7 を行なってみてください。
もはやバイナリに変化はありません。
すなわち、セルホスティング達成です。

ただし注意点として、手順 7 は Mac, Linux 系でしか行えません。
リダイレクト入力の「<」を使っているからです。
Windows の場合は、「./VSM.exe -u」と、起動オプション -u をつけて FORTH80 を起動し、start.txt の内容を丸ごとコピペして実行してください。BYE で FORTH80 を終了するときに FORTH80.bin を上書きするか聞かれる（これが -u の意味）ので、「Y」または「y」を入力してください。

ああ、重要な注意を書き忘れるところでした。FORTH80.fth の中で空行を表しているかに見える「\<n\> P  」の後ろのスペース二つを削除してしまってはなりません（もしこちらのミスで空白文字が足りない箇所があったら補ってください）。
FORTH のエディタはコマンド「P」の後ろにスペースが一つより多く続くと空行だと認識するからです。
スペースが一つまたはゼロのときは、何かイレギュラーな文字コードが表示されない形で残ることになって、ロードしたときに正しく機能しません。
自動で余計な空白を消去することができる高機能なエディタで FORTH80.fth を開くときは注意してください、

## 起動オプション

### -h
起動オプションのヘルプを表示します。

### -s
> ./VSM -s&crarr;

と、オプション -s を付けて起動すると、初めから入力として標準入力を使用する状態になります。
これは、パイプ処理やリダイレクトで使うことを想定しています。

### -u
> ./VSM -u&crarr;

でアップデートモードとして起動したことになります。
BYE で終了するときに、システムファイルを上書きするか尋ねてきます。それまでに加えた変更（例えば、ロードしたワード等）も含めてシステムの内容を FORTH80.bin に上書きするか、ということです。

### -v
バージョンを表示します（それのみで終了）。

## プログラミング言語 FORTH の初歩の初歩

### スタックと計算

FORTH は、いわゆる逆ポーランド記法を基本としています。
したがって、1234 - 34 という計算を行って結果を表示したければ、

    1234 34 - .

と入力してリターンキーを押します。

すると、

    1200 ok

と表示されます（出力が改行してから新しい行に表示されるかスペースを挟んで入力と同じ行に続くかは、入力に標準入力を使用しているかどうかで違います。「[入出力について](#入出力について)」を参照）。最後のピリオドはドットと言って、結果の数値を表示するのに用いられるワードです。ワードはプログラミング言語一般における命令と同義です。

(1234 - 34) / 2 という計算は、

    1234 34 - 2 / .

とすれば結果が表示されます。
FORTH において括弧は文や式の構造化のために必要ありません。

この計算の FORTH 内部における実行の様子は以下のとおりです。
まず、1234 がスタックという記憶領域に置かれます。
FORTH 処理系は、入力された数値はスタックに置くべきもの、と解釈します。
スタックとは本を積み重ねたようなものだとお考えください。
下の本は抜き取れないとします。
一番上に新しい本を積み重ねて、取り出すときは一番上の一冊しか取れない。
このスタックを便宜上、90度時計回りに横倒ししたものとしてここでは図式化して表します。
ここまでの処理を示すとこうなります。
式（これも FORTH プログラムの文の一種）とスタックの内容を並べて書いたものです。

    1234 34 - 2 / .
    ^^^^
    Stack: 1234

「^^^^」の部分は、「FORTH 処理系がそこまで読み込んだ」ということを意味します。
処理系が 34 まで式を読んだとき、1234 の上に 34 が積み重ねられます。

    1234 34 - 2 / .
         ^^
    Stack: 1234 34

そして、引き算を意味するワード「-」を読んだ処理系は、まず 34 を取り出してこれを引く数とし、次に 1234 をスタックから取り出してこれを引かれる数とし、1234 - 34 を計算して結果の 1200 を新たにスタックに積みます。

    1234 34 - 2 / .
            ^
    Stack: 1200

そして、2 が 1200 の上に積まれる。

    1234 34 - 2 / .
              ^
    Stack: 1200 2

「/」を読んだ処理系は、2 と 1200 をスタックから取り出して、後者を前者で割った答えの 600 をスタックに積む。

    1234 34 - 2 / .
                ^
    Stack: 600

式を読み終えた後にスタックの一番上に残っているものが答えですが、この場合、まだワード「.」が残っています。
これを先ほどは「結果を表示せよという命令」と説明しましたが、正確には「スタックの一番上の数値を表示せよ」という命令なのです。
そのようにして答え 600 が表示される。
その代わり、スタックからは 600 が取り除かれます。

    1234 34 - 2 / .
                  ^
    Stack:

ちなみに、スタックの内容はワード「.S」ですべて表示することができます。
「.」と違って「.S」はスタックを弄らないので、スタックの内容が分からなくなって困ったら、「.S」を使ってこまめに調べることをお勧めします。

言い忘れましたが、FORTH にはメインに使われるスタックの他に、補助的に使われるもう一つのスタックがあります（ただしこれはユーザー目線のことで、実はシステムはこちらのスタックも重要なことに使っている）。
前者をパラメータ・スタック、後者をリターン・スタックなどと呼びます。

### コロン定義

FORTH では、

    : 34-2/  34 - 2 / ;

のように、「:」の直後にスペースを挟んで何か数値ではない一続きの文字列を書いて、それと「;」の間に他のワードや数値をスペースで区切って並べると、その文字列を新しいワードとして定義できます。
これを**コロン定義**と言います。
ただし、未定義のワードや、ワードとも数値ともみなされないものをコロン定義のなかに書くとエラーになります。

FORTH におけるワードとはプログラムのサブルーチンと同義です。
もっと正確には、サブルーチンを呼び出すラベル名に相当するものが FORTH のワードです。
定義するワード名の後に書かれたワードや数値の並びがプログラムということになります。
FORTH におけるプログラミングとは、コロン定義を書くことです。

コロン定義されたワードは、FORTH に最初から用意されているワードと全く同様に用いることができます。たとえば上のように入力してリターンキーを押した後、

    1234 34-2/ .

と入力してリターンキーを押せば

    600 ok

と返ってきます。
「34-2/」は計算式ではなく単なるワード名だということに注意してください。

ユーザによってコロン定義されたワードについては、削除することができます。

    FORGET -34/2

で、この新ワード「-34/2」は無かったことになります。
ただし、これ以後に定義したワードも同時に失われるので注意してください。
ワードはリスト状に連なる形で管理されており、その「鎖」の一箇所を断ち切るとそこから後ろ側は全て失われるからです。
ちなみに、FORTH ではワードの上書きが可能で、つまり、同じ名前のワードを何度も定義すると、内部的には同音異義語的に同じ名前のワードが重複して記録されます。
この場合、FORGET は一番最後に定義したワード（から後）を削除します。
これは、リストを後ろから検索して最初に一致したワードに対して FORGET するからです。
したがって、同音異議語を定義した回数分も含めて FORGET をしないと、そのワードを削除できません。

### 制御構造

FORTH には「IF」や「WHILE」などのプログラムの制御構造のためのワードももちろんあります。
それらはコロン定義の中だけに書けます。

たとえば IF は、それが読み込まれたときにスタックのトップの値がゼロだったら、対応する ELSE または THEN の直後の位置に読む箇所をジャンプさせているだけです。ループ処理を担当するワードも同様です。

厳密には、コロン定義が内部言語（FORTH 版バイトコード）に逐語訳されるコンパイルの際に、そのようなことを行う単純なジャンプ命令の組み合わせに置き換えられます。

置き換えられるというより、IF はコンパイル時に THEN などといわば協働して、ジャンプ先を調整したジャンプ命令を自分の位置に組み込みます。このところに FORTH らしいプリミティブな巧妙さがあると言えば言えるでしょう。

### 変数および配列

FORTH80 の変数は 16 ビット長のものが基本です。
FORTH80 のスタックが 16 ビットの値を一つの要素としてプッシュ（スタックに積む）したりポップ（スタックから取り出す）するようにデザインされているからです。
変数は

    VARIABLE <variable_name>

で定義できます。
注意点としては、FORTH80 は FORTH-79 Standard に準拠しているので、変数を定義したときにスタックを消費しないということです。
つまり、処理系によっては、スタックから値を一つ下ろしてきて定義した変数の初期値として代入するようになっているものがあります。
この形式の違いは一見些細だけれども、別の処理系用に書かれたプログラムを読み込んだときにスタックのずれを引き起こし、それだけでプログラムは暴走してしまいます（これは FORTH の弱点です）ので、注意が必要です。

変数とは言っても、要するにメモリ領域（FORTH80 においてはもちろん実 PC のそれではない）の特定のアドレスを返すワードというのがその実態です。
空いているアドレスの一番上が割り当てられます。
そして、いわば予約済みになります。

    VARIABLE V

とした上で、

    1234 V !

とすると 変数 V に値 1234 が代入されます。

    V @

で変数 V の値 1234 がスタックに積まれます。

このような按配なので、配列の場合もこの素朴な仕組みを応用してユーザが自ら実現するだけです（注。これは FORTH80 が標榜している昔の FORTH での話であって、現代的な FORTH 処理系では多分違います）。

     VARIABLE <array_name> <bytes> ALLOT

で、要素数 \<bytes\> / 2 + 1 個の配列 \<array_name\> を定義したことになります。
というより、C 言語に馴染みのある人はすでに見当がついているでしょうが、単に「メモリ領域を確保してその先頭アドレスをポインタ変数に定数として代入した」というのと同じです。
ALLOT は「システムが次に書き込むべき位置を現在のものより x バイト後ろに変更せよ」ということを意味するワードです。
何バイトずらすかは TOS（スタックの一番上の要素）の値によって決まります。
だから、上のようにすると、変数で確保された 1 セル（2 バイトごとのメモリ領域）の後ろに追加で \<bytes\> / 2 個のセルが、システムによって勝手に書き換えられない領域として確保されて、結果的に要素数 \<bytes\> / 2 + 1 の配列として用いうるものになります。（ただし、変数はワードでもあるので、変数の実体としては 1 セルではありませんが、値を格納する部分は 1 セルで、それは変数ワードの実体の末尾に位置しています。）

また、古いFORTH における変数はすべてグローバル変数です。

### ボキャブラリ

FORTH にはボキャブラリという概念があって、

    VOCABULARY <vocabulary_name> IMMEDIATE

で \<vocabulary_name\> という名のボキャブラリを定義できます。

ボキャブラリとは FORTH において同音異義語的なワード定義に有用性を持たせる仕組みです。
基本的な FORTH ワードは、実はみな FORTH ボキャブラリに属しています。
デフォルトでは、システムは FORTH ボキャブラリを扱うようになっています。
そして、同じボキャブラリ内で「同綴異義ワード」を定義してもただ上書きするだけで、ユーザがそれらを使い分けることはできませんが、異なるボキャブラリにおいてそうすれば使い分けが可能になります。

とは言いつつも、実際の仕組みはあたかも「単語帳に付箋を貼る」ようなことで実現しています。
いわばそれらの付箋にボキャブラリ名が重複なしに書かれていて、システムはいま対象になっているボキャブラリ名の書かれた付箋の位置から前に向かって逆順にワードを一つ一つ辿って行き、最初に一致したワードを実行もしくはコンパイルするのです。
FORTH ボキャブラリは一番最初に置かれていて、すなわち、どのボキャブラリを対象にしていても検索されるが、上書きされる可能性も大きいです。

また、ボキャブラリ X よりも後ろに FORTH ボキャブラリのワードを新たに定義した場合、それだけで FORTH ボキャブラリにはボキャブラリ X のワードも含まれてしまうことになります。
これはボキャブラリという言葉から連想されることとは乖離しているので注意が必要です。

古い FORTH 処理系におけるボキャブラリの仕組みは、単に同綴異義ワードのみに効果が出さえすればよいという発想で考えられているようです。
それはワード一般の仕分けのためのものではないので、そのように理解していると混乱するでしょう。

このような次第なので、インタプリタが「?」を返してきたら、とりあえず、ボキャブラリが自分の意図したものに設定されているかどうかを疑うべきです。
とくにありがちなのは、FORTH ボキャブラリではないボキャブラリを使っていて、何かプログラムをロードした後に、今まで使えていたワードが急に未定義扱いになることです。
これはロードしたプログラムがロード時にボキャブラリを一方的に切り替えたことが原因です。

    <vocabulary_name>

と、すでに定義されているボキャブラリ名をワードとして（実際にワードです）実行すれば、そのボキャブラリが有効になります。
ところがこれだけではコロン定義のボキャブラリまでは切り替わりません。

    <vocabulary_name> DEFINITIONS

で、実行もコンパイルも \<vocabulary_name\> ボキャブラリが有効になります。

### 倍長整数と小数点「もどき」

古い FORTH 処理系は 16 ビット整数が基本ですが、32 ビットの整数も倍長整数として一応扱えます。

もっとも、ここで触れておきたいのは、利便性のアピールではなく、注意点です。

それは、例えば

    12340.0000

などと入力するとこれは自動的に倍長整数 123400000 を意味することです（12340 ではない！）。
じゃあこのピリオドは小数点ではなくただ倍長整数であることを示す記号なのかというと、そういうわけでもありません。
小数点の位置を示す DPL という専用の変数が用意されていて、上のように入力すると確かに DPL は 4 になります。

ちなみに、倍長整数どうしの演算は加減算しか実装されておらず、DPL も上記の仕組み以外の仕組みとは結びついていないから、この「小数点」は実質、算盤にマーカーペンで付けた印のようなものです。
ただ、上のような擬似的な小数表記で入力したときに自動で小数点以下の桁数が設定されることと、「これは倍長整数だよ」と処理系に示して、正しく単長整数のペアに変換させられることに意味があります。

（例えば、倍長整数 123400000 は -4288 1882 という二つの 16 ビット整数としてスタックに積まれます。
マイナスが付くのは補数表示だからで、内部的には 61248 1882 です〔1882×65536＋61248＝123400000〕。
12340.0000 と入力しても 0000 12340 とスタックに積まれるわけではないです。
それゆえ例えば、123400000. と入力しても結果は同じです。
しかしながら、こちらは DPL が 0 に設定されます。
倍長整数を入力するときは数の途中ではなく末尾に小数点もどきを付けることをお薦めします。）

それにしても紛らわしいです。
まさに「それは仕様です」（この倍長整数入力法を兼ねた固定小数点方式は）なので仕方がありませんし、確かに浮動小数点計算の実装が高くつく昔の環境下ではベターな方法だったのでしょう。
（「小数はたいてい倍長整数で扱うもの。そして、固定小数点方式において小数点はプログラマが勝手に想定する印でしかない。ならば、それに倍長整数の印としての役割も担わせてしまえ」ということだったのかもしれません。こう考えると、古典的 FORTH 処理系に触れたときにしばしば感じる、特有の「生活の知恵」のようなもの——あたかも発展途上国の庶民や自給自足に近い生活を送っている人々の間で見られるような——を、個人的にはこれにも感じなくはないです。）

ともかくも、小数を試しに入力してみて「計算がおかしい」と目くじらを立てないでください。
それの実態は倍長整数で、小数点以下の桁数はユーザ変数 DPL に入っています。
なおかつ、小数の加減算の入力を行うときは、小数点以下の桁数を揃える必要があります。

### 文字列

FORTH80 における文字列は Pascal 文字列です。
すなわち、文字列本体の先頭に文字列の長さを表す 1 バイトが付け足されています。
文字列末尾にヌル文字は必要ありません。

文字列の直接的表示にはワード「."」を用います。

    ." Hello, World!"

で、「Hello, World!」と表示します。注意点は、後ろの「"」はワードではないことです。ワード「."」は、自分の後ろの記号「"」までの文字列を表示すべき文字列とみなします。

### 外部記憶装置

古い FORTH 処理系における二台のフロッピーディスク・ドライブに相当するものを、FORTH80 では DRIVE0.img と DRIVE1.img というファイルで仮想的に置き換えてあります。
ファイル名は固定されており、また、フロッピーの入れ替えに相当することはできません。
もちろん、リネームすれば結果的に入れ替えたことになります。
（この辺は、もしかしたら改良するかもしれませんが、自分的には「使う人がソースコードを勝手にいじったらいい」と思っています。それは C 言語のソースコードのほうに書かれていて、簡単な仕組みしか使っていません。）

古い FORTH 処理系に倣って、FORTH80 も（仮想）フロッピーディスクにデータをいわばベタ書きします。
すなわち、ファイルやディレクトリなどというものはありません。
その代わり、スクリーンという一種の単位によってディスクの全領域は分割されて管理されます。
喩えるなら、マス目付きの原稿用紙のようなものです。
ファイルもフォルダもなくてただマス目付き原稿用紙の束がある。
1 スクリーンは 1024 バイトです。

     <screen_number> LIST

で、番号 \<screen_number\> のスクリーンを見ることができます。

> DR1&crarr;

とすると、\<screen_number\> がドライブ 1 のスクリーン番号を意味するようになります。

> DR0&crarr;

でデフォルトに戻ります。
デフォルトでは、\<screen_number\> はドライブ 0 のスクリーン番号を意味します。
このとき、\<screen_number\> がドライブあたりのスクリーン数 N 以上である場合は、ドライブ 1 のスクリーン番号 \<screen_number\> - N を意味します。
ドライブ 1 でも実は同様ですが、FORTH80 のシステムにおいてドライブ 2 は存在しないので、単にエラーになります。

ドライブ 0 の番号 0 から 3 までのスクリーンはいわば欠番で、何も書き込まないようにします。
これはどうも、CP/M バージョンの Fig-FORTH という FORTH 処理系での慣習らしいです。
欠番にしておかないとどうやら CP/M のシステムディスクに入れ替えたとき CP/M がフリーズする。
（これは私もエミュレータでですが遭遇しました。原因は、CP/M に詳しくない私にはわかりません。）
FORTH80 は CP/M をエミュレートしているわけではないのでこの措置は必要ありませんが、DRIVE0.img には今のところなんとなくこの「慣習」が残っています。

    <screen_number> LOAD

で \<screen_number\> のスクリーンに書かれている FORTH プログラムをロードすることができます。
スクリーンのプログラム末尾にワード「-->」が書かれていると、次のスクリーンも自動で読み込みます。
「;S」を読むかスクリーン末尾にまで到達するとロードは終了します。

DRIVE1.img の中身は空です。
DRIVE0.img にはスクリーン 8 からユーティリティの類が収録してあります。
スクリーン 6 にはロード命令それ自体が書かれていて、すなわち、このスクリーンをロードすると他の一連のスクリーンに書かれているエディタ・プログラムが読み込まれるようにしてあります。
これを使えば仮想ディスクの内容を書き換えることができます。
エディタの使い方は [editor_ref.md](editor_ref.md) を参照してください。

この節の説明はいかにも不完全でつまみ食い的なものですがこの辺で終わりにさせていただきます。
ここで触れたことは、是非とも説明しておかないとおそらく徹底的に FORTH 理解の障害になってしまう類の事柄なのですが、逆にそれさえ説明してしまえば、「このドキュメントを読んでいる人は FORTH に真剣な興味を持っているはず」という仮定にいわば甘えることができるからです。

FORTH の体系だった詳細を知りたい方には、「[これは何？](#これは何)」の節でも挙げた井上外志雄『標準 FORTH』を第一にお薦めしますが、古本で手に入れるのも図書館で借りるのも難しいかもしれません。
そこで、（今のところ）確実にしかも無料で入手可能なものとしては、英語になってしまいますが Leo Brodi ["Starting FORTH"](https://www.forth.com/starting-forth/) をお薦めします。それの特に First edition の PDF がいいです。
First edition を薦めるわけは、そこで説明されているフロッピーディスク上のデータへのアクセス形式が FORTH80 のものと同じだからです。
しかしながら、そこで説明されているエディタの形式が『標準 FORTH』におけるものと違います。
そちらのエディタも DRIVE0.img のスクリーン 27 以降に収録してあります。
ただし、スクリーン 27 をエラーなくロードするには、前もってスクリーン 16 をロードしておく必要があります。

## 入出力について

FORTH80 は実質、エミュレータなので、標準入出力と結びつくことで初めて実 PC に作用を及ぼすことができます。
それゆえ、少々イレギュラーな標準入出力の用い方をしています。

FORTH80 はデフォルトでは標準エラー出力に出力します。

> 1 PFLAG !&crarr;

と入力して値 1 をユーザ変数 PFLAG に設定すると、標準エラー出力に加えて標準出力にも出力を行います。
これは、古い FORTH 処理系におけるモニタ出力とプリンタ出力とをそのまま標準エラー出力と標準出力とに置き換えたものです。
（プリンタ出力のように標準出力の出力先を画面以外にすることを想定しています。
標準エラー出力は画面以外に出力先を変えられないからです。）
ちなみに、値は非ゼロならば 1 でなくても構いません。
一方、

> 0 PFLAG !&crarr;

と入力すれば、FORTH80 は即座に標準出力への出力を停止します。

入力についても FORTH80 は２系統持っています。
C 言語の getch 関数、またはそれ相当の仕組みで標準入力を介さず直接的にキー入力を受け付けるものと、標準入力を使うものとです。
これらは出力の２系統と違って、両立はしません。

> 0 STDIN !&crarr;

で前者に、

> 1 STDIN !&crarr;

で標準入力になります。
1 でなく非ゼロの値でも可。

また、標準入力を使わないときは FORTH80 自身にキー入力のエコーをさせねばなりませんが、それだと標準入力のときは入力がオウム返しになって煩わしいです。

> 0 ECHO !&crarr;

でエコーを表示させなくすることができます。

> 1 ECHO !&crarr;

で表示するようになります。
1 でなく非ゼロの値でも可。

注意。
上記の変数の設定の組み合わせによっては、入力や出力が表示されなくなったり、文字がダブって表示されたりします。
標準出力、標準エラー出力、標準入力、getch() 関数的な入力、これらの意味と今使っているのはどれなのかに留意して、ご自分で考えてご使用ください。

## 独自の FORTH ワード

FORTH80 にはいくつか拡張的に付け足した FORTH ワードがあります。
以下に見られる「(x1 x2 ... --- y1 y2 ... )」なる記法は、そのワードがスタックから値 x1, x2, ... を pop して、値 y1, y2, ... をスタックに push することを意味します。
なお、厳密な記法というわけではありませんが、値が アドレス（16ビット）を意味するときは a と表記し、16 ビットの数値のときは n，8 ビットの数値のときは c，32 ビットの数値のときは d としばしば表します。

### PFLAG

( --- a )</br>
ユーザ変数です。
「入出力について」のところで説明したように、これは標準出力に出力するかどうかを決めるためのフラグです。

### UTF-8

( --- a )</br>
これもユーザ変数なので、自身のアドレス a を返します。

文字コードとして UTF-8 を用いるかどうかを決めるためのフラグです。
非ゼロに設定すると、FORTH80 は UTF-8 に適したやり方でマルチバイト文字を処理します（日本語環境の漢字／仮名でしか機能しないかもしれません）。

### ECHO

( --- a )</br>
ユーザ変数です。
入力をエコーするかどうかのフラグ。
このエコーはターミナルの行うそれではなくて、FORTH80 が行うそれです。
フラグの値はアドレス a に格納されています。

### UVR

( --- n )</br>
これは定数であり、ユーザ変数の初期値が書かれたメモリ領域の先頭のアドレス n を返します。
n と表記するのはこれが定数のためです。

FORTH80 の起動時に、ユーザ変数にそれぞれの初期値が読み込まれます。
ユーザ変数は後からいくらでも値を書き換えられますが、ユーザ変数に対する通常の操作で初期値自体が書き換わることはありません。
それら初期値を変更するためにこの定数 UVR が必要になります。

### SCR/DRIVE

( --- n )</br>
仮想フロッピードライブあたりのスクリーンの数を意味する定数です。

スクリーンとは、古い FORTH 処理系における外部記憶装置のデータ管理で用いられる単位です。
スクリーンあたり 1024 バイトです。

### POPEN

( a1 a2 --- ud1 ud2 )</br>
これらは C 言語における popen 関数を模したワードです。実際にそれを使って実装してあります。
ud と記したものは 符号なし 32 ビットの数値です。本当は ud1 と ud2 で 64 ビットの一つの数値です。

つまり、これは C 言語の popen そのままに、文字列の先頭アドレス二つを引数にとって FILE ポインタの値を返すものです。
しかし FORTH80 には FILE 構造体などという高級なものはないので、単に倍長整数のペアとして扱うしかありません。

ただし、FORTH80 における文字列はいわゆる Pascal 文字列です。
つまり、先頭１バイトが文字列本体の長さを表します。

a1 は OS のコマンドを表す文字列の先頭アドレスで、a2 は モードのそれです。
モードとは "w", "r", "r+" のことです。
"r+" については環境によって機能したりしなかったりするでしょう。
これは FORTH80 の問題ではなく、それのバーチャルマシンを記述するのに用いた C 言語の問題です。

**POPEN は C 言語の popen と同様、実行したら、その返すポインタを引数として PCLOSE をしなければなりません。**

#### 使い方の例

まず、FORTH80 を起動して、次の FORTH プログラムを入力します。
ちなみに、スペースを伴った左丸括弧「(」はこれでも立派な FORTH ワードで、これが FORTH におけるコメント文の開始記号です。
 記号「)」（こちらは FORTH ワードではない）までを読み飛ばします。

    : <TEXT: ( --- )
      QUERY 0 WORD SWAP OVER C@ 1+ CMOVE ;

    : DD! ( ud1 ud2 a --- )
      0 3 DO
        SWAP OVER I 2* + ! -1 +LOOP
      DROP ;

    : DD@ ( a --- ud1 ud2 )
      4 0 DO
        DUP I 2* + @ SWAP LOOP
      DROP ;

    VARIABLE VAR64_A 7 ALLOT

    : OUT> ( --- )
      1 STDIN !
      POPEN VAR64_A DD! VAR64_A DD@ SET-INPUT
      BEGIN KEY DUP 255 < WHILE EMIT REPEAT
      0 0 0 0 SET-INPUT DROP
      VAR64_A DD@ PCLOSE 2DROP ;

（長い定義——すなわち FORTH におけるプログラム——は 80 文字を超えない範囲で適度に改行してください。
さもないと、ワードの途中で 80 文字を超えて強制改行され、未定義のワードがあるということでエラーを出されて入力がなかったことになります。）

真ん中あたりで、VAR64_A という名前の変数を定義しています。ALLOT で 7 バイト追加して、8 バイト確保しています。

> VAR64_A DD!&crarr;

でスタックにある 16 ビットの値4個ぶんが変数 VAR64_A に格納されます（アドレスの大きい方から埋まる）。一見、直感に反しますが、ud1 が下位 16 ビット、ud2 が上位です。

> VAR64_A DD@&crarr;

で VAR_64_A の中の値がスタックに書き出されます（アドレスの小さい方から取り出される）。

このように、面倒でも変数を用意して POPEN の返す値をさっさと格納してしまった方が、スタック操作をミスして値を破壊してしまう恐れがなくて安全です。

"255" は文字コード 0xFF を意味しています。
ワード名は重複しさえしなければ何でもよく、一例に過ぎません。

さて、このように定義した上で、「PAD \<TEXT:」と入力してリターンキーを押すと「ok」の表示が出ずにカーソルがワードのすぐ後ろ（標準入力使用時は次の行の先頭）で止まって更なる入力待ちになります。
そこで、コマンドを表す文字列を入力します。
つまり、

> PAD \<TEXT:&crarr; \<command\>&crarr;

で、文字列 \<command\>（スペースを含んでもよい）が FORTH80 システム内のメモリの PAD と言われる場所に格納されます。
ただし、PAD の仕様上、80 文字までしか推奨されません。
これは、FORTH80 において改行なしに入力できる最大文字数が 80 文字だからです。
それゆえ、メモリ領域を別途に確保し、リターンキーごとに入力された文字列を繋げて保存する FORTH ワードを定義すれば、もっと長い文字列もイレギュラーに破壊される恐れなく扱えます。
しかし、Pascal 文字列の制約上、最大でも 255 文字であることに注意してください。

たとえば、

> PAD \<TEXT:&crarr; ls -l&crarr;

などとします（Mac/Linux の場合。Windows では "dir"）。

そして、

> PAD "r" OUT>&crarr;

もしくは

>PAD "r" CR OUT>&crarr;

とすれば、カレントディレクトリの内容が、FORTH80 であるはずの画面にずらずらと表示されるはずです。
「CR」 は改行を出力するワードです。
標準入力のときは入力の後にいちいち改行が入るので前者を、そうでないときは後者を使うと表示が整います。
OUT> 実行後は入力が一律に標準入力に切り替わっているので必要があれば戻してください。
ここで "r" は二重引用符も含めて一つの FORTH ワードです。
ただし、これも独自ワードです。
POPEN のために一文字の文字列 r を特別に用意してあって、その先頭アドレスを返すワードです。

注意点があって、**POPEN はサブプロセスを発生させるもので、なおかつ、入出力を弄るものであるから、不用意に使うと思わぬ結果やトラブルをもたらしかねない**、ということです。
これは、以下に説明する PCLOSE, SET-INPUT, SET-OUTPUT にも共通して言えます。
上記の例でも、OUT> の定義から「1 STDIN !」を取り除いたものを標準入力でないときに実行すると､キー入力はできるのに FORTH80 が一切無反応になって、強制終了するしか手段がなくなります。

### "r"

( --- a )</br>
二重引用符も含めてワードです。
POPEN でモードを指定するためにあらかじめ用意されている１文字の文字列 r の先頭アドレス a を返します。
Pascal 文字列なので、メモリ上では 2 バイトの長さです。

### "w"

( --- a )</br>
POPEN のための１文字の文字列 w の先頭アドレス a を返します。
メモリ上では 2 バイト。

### "r+"

( --- a )</br>
POPEN のための２文字の文字列 r+ の先頭アドレス a を返します。
メモリ上では 3 バイト。

### PCLOSE

( ud1 ud2 --- d )</br>
PCLOSE は、ud1, ud2 の値がポインタとして指し示すプロセスを閉じます。
ud2 × 2<sup>32</sup> + ud1 がポインタの実際の値です。
戻り値は本当は int ですが、FORTH80 では倍長整数としてしか扱えない。
PCLOSE は、要するに C 言語の pclose そのままです。

### SET-INPUT

( ud1 ud2 --- d )</br>
ud2 × 2<sup>32</sup> + ud1 がポインタとして指し示すものを、標準入力の入力元とします。
NULL の場合はデフォルトのものに戻します。

### SET-OUTPUT

( ud1 ud2 --- d )</br>
ud2 × 2<sup>32</sup> + ud1 がポインタとして指し示すものを、標準出力の出力先とします。
NULL の場合はデフォルトのものに戻します。

## 定義しておくと便利なワード

 作者自身の備忘録も兼ねて、定義しておくと便利な FORTH ワードを挙げておきます。
 いずれも、ワード名は単なる一例です。

### {, }

    : { ( --- ) 1 PFLAG ! ;

    : } ( --- ) 0 PFLAG ! ;

このように定義すると、ワード「{」とワード「}」に挟まれた FORTH プログラムは出力先が標準出力になります。

### .\

    : .\ ( --- ) S->D 0 D.R ;

このワード「.\」は、ワード「.」と同等の機能で、つまり、値を数字表現として（そのとき FORTH80 で設定してある進数に応じて）表示するものです。
ただ、後ろにスペースが出力されないかどうかのみが異なります。「.\」はスペースなしです。

そもそも、初めから「.」をスペースなしに設定しておいたほうがいろいろと融通が効いたのではないか、と思うのですが、なぜか FORTH のデファクトスターンダード的に「.」はスペース付き出力になっている。

### HOME, CLS

    : HOME ( --- ) 27 EMIT ." [0;0H" ;

    : CLS  ( --- ) HOME 27 EMIT ." [2J" ;

HOME はカーソルを画面の左上端に移動させ、CLS はそうしたうえで画面を消去するワードです。

ただし、「ok」の表示が出るのでカーソルは完全に一番上の行に来るのではありません。

これらは要するに、ANSI エスケープシーケンスのカーソル位置指定コマンドと画面消去コマンドを使っているだけです。
ところが、１行あたりの文字数が固定されていてスクロールも自由にできない昔の PC とそうではない現代の PC とでは「画面」の意味合いが異なってしまっているようです。
つまり、カーソルの戻る範囲および画面の消せる範囲が限定されています。

さらに、Windows では、コンソールが ANSI エスケープシーケンスを受け付けないので、レジストリをいじるなどして設定を変えないといけないようです。

----
