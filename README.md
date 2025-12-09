# Rust版 MinCaml コンパイラ 実装解説

このドキュメントでは、Rustで実装されたMinCamlコンパイラの概要と使用方法について説明します。このコンパイラは、MinCamlのソースコードをクロージャ中間表現（Closure IR）に変換します。

## 実装されたステージ

1.  **字句解析・構文解析 (`src/parser.rs`)**:
    -   `nom` ライブラリを使用して実装されています。
    -   `let`, `let rec`, `if`, `let tuple` などの基本的な構文をサポートしています。

2.  **型推論 (`src/typing.rs`)**:
    -   単一化（Unification）を用いた型推論を実装しています。
    -   型変数は `Rc<RefCell<Option<Type>>>` を用いて管理されています。

3.  **K正規化 (`src/k_normal.rs`)**:
    -   型付きASTをK正規形（ネストされた `Let` 式）に変換します。
    -   これが「A正規化」に相当します。

4.  **α変換 (`src/alpha.rs`)**:
    -   プログラム中の変数を一意な名前にリネームします。

5.  **クロージャ変換 (`src/closure.rs`)**:
    -   関数をクロージャに変換し、自由変数をキャプチャします。

6.  **デバッグ出力 (`src/debug.rs`)**:
    -   Closure IRを人間が読みやすい形式で出力します。

## 使用方法

コンパイラを実行するには、以下のコマンドを使用します。

```bash
cargo run -- <ファイル名>
```

### 実行例

入力 (`test3.ml`):
```ocaml
let rec fib n =
  if n <= 1 then n
  else fib (n - 1) + fib (n - 2)
in
print_int (fib 10)
```

出力:
```
LetRec{
name: (fib.10, (int -> int));
args: [(n.11, int)];
body: 
  int Ti3.12 = 1;
  If (n.11 <= Ti3.12) {
    n.11
  }
  else {
    int Ti4.13 = 1;
    int Ti5.14 = n.11 - Ti4.13;
    int Ti6.15 = fib.10[[Dir]](Ti5.14);
    int Ti7.16 = 2;
    int Ti8.17 = n.11 - Ti7.16;
    int Ti9.18 = fib.10[[Dir]](Ti8.17);
    Ti6.15 + Ti9.18
  }
}
int Ti1.19 = 10;
int Ti2.20 = fib.10[[Dir]](Ti1.19);
min_caml_print_int[[Dir]](Ti2.20)
```

## テストファイル

以下の3つのテストファイルを作成しました。

1.  `test1.ml`: 単純な算術演算
2.  `test2.ml`: 関数定義と適用
3.  `test3.ml`: 再帰関数（フィボナッチ数列）
# mincaml_to_bf
