# Claude Code の設定

Claude Code CLI でプロンプトを入力中に `C-x C-e` を押すと、tmux の**フローティングペイン**で
`emacs -nw` が開き、編集した内容がプロンプトに戻るようにする設定。

- Emacs は呼び出した Claude のペインの上に開く。マウスで動かしたり、大きさを変えたりできる。
- Emacs を開いている間も、後ろの Claude のペインの表示が残る。
- 既定でエディタを開く `C-g` は、Emacs の `C-g` と紛らわしいので外している。

## 前提

- tmux 3.8 以降（フローティングペインを使う）
- bash（macOS 標準の 3.2 で動く）、GNU Emacs（`emacs` が PATH にあること）

## ファイル

| ファイル | 置き場所 | 内容 |
| --- | --- | --- |
| `claude-notepad` | `~/.local/bin/claude-notepad` | Claude Code から呼ばれるエディタ |
| `keybindings.json` | `~/.claude/keybindings.json` | `C-g` のエディタ起動を外す |
| `claude-notepad-popup` | `~/.local/bin/claude-notepad-popup` | tmux 3.8 未満用（補足を参照） |

## 設定方法

```sh
cp claude/claude-notepad ~/.local/bin/
chmod +x ~/.local/bin/claude-notepad
cp claude/keybindings.json ~/.claude/keybindings.json   # 既にある場合は中身をマージする
```

`~/.claude/settings.json` の `env` に次を足す（`<HOME>` は自分のホームディレクトリの絶対パス。`~` は展開されない）。
Claude Code の中だけで効き、普段のシェルの `$EDITOR` / `$VISUAL` には影響しない。

```json
"env": {
  "VISUAL": "<HOME>/.local/bin/claude-notepad",
  "EDITOR": "<HOME>/.local/bin/claude-notepad"
}
```

`settings.json` と `keybindings.json` を変えたら、Claude Code を起動し直す。

`VISUAL` / `EDITOR` を設定しないと、Claude Code は PATH にある `code`、`vi`、`nano` の順で最初に見つかったもので開く
（git の `core.editor` は見ない）。

## 使い方

1. Claude Code のプロンプトで `C-x C-e` を押す。
2. 枠の上に ` Claude prompt (<ペイン番号>) ` と出たフローティングペインで、Emacs が開く。
3. 編集して保存し、`C-x C-c` で Emacs を終了すると、内容がプロンプトに戻る。

- 保存せずに終了すると、プロンプトは元のまま。
- 枠の `[x]` などでペインごと閉じても、Claude Code は固まらずに戻る（中の Emacs は止める）。
- tmux の外で Claude Code を動かしているときは、同じ端末でふつうに `emacs -nw` が開く。

## 注意

- 名前に `notepad` を含めているのは、Claude Code に GUI エディタと判定させるため。
  GUI エディタでないと判定されると、Claude Code はエディタを起動する前に自分の画面を消すので、
  後ろの Claude のペインが空になる。名前を変えるときは `notepad` を残す（`code` は含めない）。
- GUI エディタと判定されるので、ファイルを開くだけの場面（`/memory` など）では Claude Code は Emacs の終了を待たず、
  開いている間も Claude を操作できる。行番号の指定（`+行番号`）は渡されない。
- tmux の外では、ファイルを開くだけの場面で Emacs が開かない（端末から切り離して起動されるため）。

## 補足: tmux 3.8 未満で使う

フローティングペインは tmux 3.8 から使える。それより古い tmux（3.3 以降）では、
`claude-notepad` の代わりに `display-popup` で Emacs を開く `claude-notepad-popup` を使う。

```sh
cp claude/claude-notepad-popup ~/.local/bin/
chmod +x ~/.local/bin/claude-notepad-popup
```

`settings.json` の `VISUAL` / `EDITOR` は `<HOME>/.local/bin/claude-notepad-popup` にする。

フローティングペイン版との違い:

- ポップアップは動かしたり、大きさを変えたりできない。
- 開いている間は、ほかのペインを操作できない（キーはすべてポップアップに行く）。
- 枠の `[x]` などのボタンはない。
