# FORTH80

### Abstract

An emulator of the programming language FORTH.

FORTH80L compliants with FORTH-79 standard.

----
## これは何？

FORTH というプログラミング言語の処理系の一種のエミュレータです。ただし、あえて古い形式のものをエミュレートしています。

FORTH-79 Standard に準拠しています。

これは処理系そのものではなくそのエミュレータであることに注意してください。しかしながら、ターミナルをいわば遠隔操作する機能を付加したことにより、まったく孤立した「箱庭」ではなくなったので、スクリプト言語のように使うことも不可能ではないかもしれません。

井上外志雄『標準 FORTH』(共立出版，1985）で解説されている FORTH 言語の仕様に従っています。それゆえ、この書籍を読む際には少なからず役立つと思います。

## 使い方

以下のファイルを同一フォルダ内に置いてください。専用のフォルダを用意することをお勧めします。

- VSM, VSM.exe のどちらか一方
- FORTH80.bin
- DRIVE0.img
- DRIVE1.img

VSM.exe は Windows 用、VSM は Mac 用です。Mac 版は再コンパイルすれば（ソースファイル VSM_mac.c。ただし同梱の conio.h が必要） Linux でも動くと思います。

Windows の場合は、VSM.exe をクリックすればコンソール画面が立ち上がります。Mac の場合は、ターミナルから ./VSM と入力して起動しないとダメです。

それ以前に、「このアプリは安全が保証されていないので実行中止」の趣旨のダイアログ、もしくはメッセージが出るかもしれません。そのときは、Windows の場合は、ダイアログの「詳細」のところをクリックして出てくる「実行」ボタンをクリックしてください。Mac の場合は、Ctrl キーを押しながら VSM をクリックして「開く」を選択、すると「本当に開いていいか」という趣旨のダイアログが出るので、「開く」をクリックしてください。Mac だとそれでもファイルの実行権限まわりのセーフティ機能が働くのか、VSM が FORTH80.bin を開けずにすぐ終了してしまいます。ですが、ターミナルから ./VSM と入力して開くと正常稼働します。いずれの場合も、以降は OS によって実行禁止されることはありません。

***注意：使用は自己責任で！***

***CAUTION: Use at your own risk!***

## 独自の FORTH ワード

FORTH80 にはいくつか拡張的に付け足した FORTH ワードがあります。

### PFLAG ( --- a )

これは FORTH のユーザ変数の一つです。古い既存の処理系に見られるもので、厳密にはオリジナル要素というわけではありません。ただ、FORTH80 においてはイレギュラーな使い方を想定してあります。自身のアドレス a を返します。

PFLAG は、本来はプリンタ出力に関するフラグであり、これに非ゼロの値を設定すると、モニタと同時にプリンタにも出力を行うようになっていたようです。

FORTH80 においては、PFLAG は標準出力を行うかどうかのフラグです。すなわち、PFLAG にゼロでない値を設定すると FORTH80 は標準出力（のみ）に出力します。

では、デフォルトではどこに出力するかというと、標準エラー出力（のみ）に対して行います。

これは、FORTH80 がプログラミング言語処理系であり、処理結果以外のエラーメッセージや「ok」等のいわば余計な表示をも出力するので、それらを篩にかけるための苦肉の策です。

つまり、それら「余計な」（もちろん実際は必要な）メッセージ出力を伴う言語処理系のインタプリタの出力全体を考えたとき、いわゆる地の文はエラーメッセージや「ok」のほうであり、むしろデータ出力は鍵括弧で囲まれた会話文に相当するものである、と。なぜなら、地の文は会話文に対して何らかのコメントをつけたりできるけれども、その逆は絶対にないからです。

それゆえ、地の文はいわばエラーメッセージなのだから、標準エラー出力がふさわしく、従って、デフォルトではすべてそちらに出力するようになっていて、必要に応じて標準出力に切り替える、という流儀のほうが正当性がある、と、作者であるこの私は（今のところ）考えております。

たとえば、次のように FORTH ワードをユーザ定義します。

    : {  1 PFLAG ! ;

    : }  0 PFLAG ! ;

すると、ワード "{" と "}" の間に書かれた FORTH フログラムの行う出力は、標準出力になります。


### UVR ( --- n )

これは定数であり、ユーザ変数の初期値が書かれたメモリ領域の先頭のアドレス n を返します。

FORTH80 の起動時に、ユーザ変数の初期値が読み込まれます。ユーザ変数は後からいくらでも値を書き換えられますが、ユーザ変数に対する通常の操作で初期値が書き換わることはありません。それら初期値を変更するためにこの定数 UVR が必要になります。

### SCR/DRIVE ( --- n )

仮想フロッピードライブあたりのスクリーンの数です。

スクリーンとは、古い FORTH 処理系における外部記憶装置のデータ管理で用いられる単位です。スクリーンあたり 1024 バイトです。

### POPENRCLOSE ( a --- )

これは C 言語における "r" モードの popen 関数と pclose 関数とを模したものです。というより、実際にそれらを使って実装してあります。

FORTH80 上から OS のターミナルにコマンドを入力し、その出力を取得するために用います。

アドレス a はコマンド文字列の先頭アドレスです。ただし、先頭には文字列の長さが格納されており、すなわち、これは Pascal 文字列です。

そして、スタック上には何も返しません。

ターミナルの標準出力をわざわざ FORTH80 の入力に戻す形で実装しています。それはこういうわけです。人間がターミナルを操作するときは、コマンドを入力し、その出力結果に応じてまた何らかのコマンドを入力する、ということを繰り返すわけですが、そこでは結果の情報を得るということが決定的に重要です。たとえば、ディレクトリの内容が分からなければファイル操作は行いようがありません。そのファイル操作のようなことを、上手く工夫すれば FORTH80 でもできるように、たとえ実際的ではなくても原理的には可能なようにしておくべきではないか、と思ったのです。

現時点では作者のこの私は単にターミナルコマンドの出力を FORTH80 内で見ることができるようになっただけです。そして、実際に FORTH80 をターミナル代わりに使ってファイル操作をガンガンやってやろう、などとは考えておりません。が、FORTH80 から何らかのコマンドを OS 側に受け渡してその実行結果を FORTH80 内部で見る、ということは（ちっとも自動化になっていないけれど）必要最低限なことなので、そのやり方を述べておきます。

FORTH80 にて、二つの FORTH ワードを次のようにユーザ定義します。

    : CMD:  QUERY 0 WORD PAD OVER C@ 1+ CMOVE ;

    : OUT:  PAD POPENRCLOSE BEGIN KEY DUP 255 < WHILE EMIT REPEAT DROP ;

ワード名は重複しさえしなければ何でもよく、一例に過ぎません。さて、このように定義した上で、CMD: と入力してリターンキーを押すとカーソルがワードのすぐ後ろで止まって更なる入力待ちになります。そこで、コマンドを表す文字列を入力します。つまり、

CMD:&crarr; \<command\>&crarr;

で、文字列 \<command\>（スペースを含んでもよい）が FORTH80 システム内のメモリの PAD と言われる場所に格納されます。ただし、PAD の仕様上、80 文字までしか推奨されません。メモリ領域を別途に確保すればもっと長い文字列も扱えますが、Pascal 文字列の制約上、最大でも 255 文字であることに注意してください。

たとえば、

CMD:&crarr; ls -l&crarr;

などとします。

その上で、

OUT:&crarr;

とすれば、カレントディレクトリの内容が、FORTH80 であるはずの画面にずらずらと表示されるはずです。