# エディタのリファレンス

- EDITOR
    + EDITOR
    + CONTEXT を EDITOR ボキャブラリに切り替える。
- LIST ( n --- )
    + n LIST
    + スクリーン n のリストを画面に表示するとともに、ユーザー変数 SCR を n に代入することによってそのスクリーンを編集対象とする。
- HELP ( n1 n2 --- )
    + HELP
    + スクリーンからのロード中にエラーによってプログラムが中断されたとき、スタック上には２つのデータ、すなわち、エラー検出時のカーソル位置 n1 およびスクリーン番号 n2 が残される。HELP はこれらのデータに従ってそのスクリーンとカーソル位置を表示する。
- L
    + L
    + 変数 SCR の内容によって指定されたスクリーンのリストを表示する。
- P ( n --- )
    + n P \<text\>
    + text を現在編集中のスクリーンのライン n に置く。ラインは改行で終わる。
- E ( n --- )
    + n E
    + ライン n を消去する。そのラインは空白になる。ライン n の内容は保存されない。
- N
    + N
    + 現在のカーソル位置からはじめて PAD にある \<text\> と一致する文字列を見つけ、カーソルをその直後に移動させる。ふつう F の後で用いる。
- B
    + B
    + PAD にある \<text\> の長さ分だけカーソルを戻す。
- X
    + X \<text\>
    + 現在のカーソル位置から前方に向かって \<text\> と一致する文字列を見つけ、それを消去する。
- C
    + C \<text\>
    + 現在のカーソルの位置からはじめて、\<text\> を挿入する。
-  K
    + K
    + 現在のカーソルの位置から PAD の \<text\> を前に戻って削除する。
- TILL
    + TILL \<text\>
    + 現在のカーソル位置から \<text\> まで消去する。
- D ( n --- )
    + n D
    + ライン n を削除する。ライン n のコピーは PAD に残る。ライン n 以下のラインは１行ずつ繰り上げられる。
- S ( n --- )
    + n S
    + ライン n 以下のすべてのラインを１行ずつ繰り下げる。ライン 15 は失われる。ライン n はすべてブランクとなる。
- H ( n --- )
    + n H
    + ライン n をPAD に保存する。ライン n の内容は変化しない。
- I ( n --- )
    + n I
    + PAD にあるテキストをライン n に挿入する。旧ライン n と以下すべてのラインを繰り下げる。ライン 15 は失われる。
- J ( n --- )
    + n J \<text\>
    + n 行目に \<text\> を挿入する。現在の n 行以下の各行は１行分だけ下へ送られ、15 行は失われる。
- CLEAR ( n --- )
    + n CLEAR
    + スクリーン n をクリアする。画面の全部がブランクとなる。
- CLEARS ( n1 n2 --- )
    + n1 n2 CLEARS
    + スクリーン n1 から n2 までをクリアする。
- COPY ( n1 n2 --- )
    + n1 n2 COPY
    + スクリーン n1 をスクリーン n2 にコピーする。n2 スクリーンの元の内容は失われる。
- COPIES ( n1 n2 n3 --- )
    + n1 n2 n3 COPIES
    + スクリーン n1 から n2 までをスクリーン n3 以降にコピーする。現在のスクリーン n3 からスクリーン n3+n1-n2 までの内容は失われる。
- T ( n --- )
    + n T
    + ライン n がタイプされる。そのラインのコピーは PAD に蓄えられる。
- R ( n --- )
    + n R
    + ライン n を PAD にあるテキストに置き換える。PAD にあるテキストはそのまま保存される。
- TOP
    + TOP
    + カーソルをスクリーンの最初の行の最初の文字の前まで動かす。
- Q ( n1 n2 n3 n4 --- )
    + n1 n2n n3 n4 Q
    + スクリーン n1 の n2 行から n3 行までの内容を現在のスクリーンの n4 行以降へ引用する。現在の n4 行以下の各行は n2-n1+1 だけ下へ送られ、15-n3-n2 行から 15 行までは失われる、
- M ( n --- )
    + n M
    + カーソルを n スペース移動させる。n が正ならばカーソルは右へ、負ならば左へ移動したのちカーソルとともにそのラインを表示する。
- F
    + F \<text\>
    + 現在のカーソルの位置からはじめて \<text\> を見つける。