# Claude Code の設定

Claude Code CLI でプロンプトを入力中に `C-x C-e` を押すと、tmux の**フローティングペイン**で
`emacs -nw` が開き、編集した内容がプロンプトに戻るようにする設定。

- Emacs は呼び出した Claude のペインの上に開く。マウスで動かしたり、大きさを変えたりできる。
- Emacs を開いている間も、後ろの Claude のペインの表示が残る。
- 既定でエディタを開く `C-g` は、Emacs の `C-g` と紛らわしいので外している。

## 前提

- tmux 3.8 以降（フローティングペインを使う）
- zsh、GNU Emacs（`emacs` が PATH にあること）

## ファイル

| ファイル | 置き場所 | 内容 |
| --- | --- | --- |
| `claude-editor` | `~/.local/bin/claude-editor` | Claude Code から呼ばれるエディタ |
| `keybindings.json` | `~/.claude/keybindings.json` | `C-g` のエディタ起動を外す |
| `../.tmux.conf_v3` | `~/.tmux.conf` | `C-x C-e` で Claude の画面を保存する bind などを含む |

## 設定方法

```sh
cp claude/claude-editor ~/.local/bin/
chmod +x ~/.local/bin/claude-editor
cp claude/keybindings.json ~/.claude/keybindings.json   # 既にある場合は中身をマージする
cp .tmux.conf_v3 ~/.tmux.conf
tmux source ~/.tmux.conf
```

`~/.claude/settings.json` の `env` に次を足す（`<HOME>` は自分のホームディレクトリの絶対パス。`~` は展開されない）。
Claude Code の中だけで効き、普段のシェルの `$EDITOR` / `$VISUAL` には影響しない。

```json
"env": {
  "VISUAL": "<HOME>/.local/bin/claude-editor",
  "EDITOR": "<HOME>/.local/bin/claude-editor"
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

- Claude Code はエディタを起動する前に自分の画面を消すので、tmux の bind で消される前の画面を
  バッファ `claude-screen-<ペインID>` に保存し、`claude-editor` が書き戻してから消している。
  エディタが起動しなかった場合（プロンプト以外の場面で `C-x C-e` を押したときなど）はバッファが残り、
  `choose-buffer`（prefix `=`）で見える。気になるときは `tmux delete-buffer -b claude-screen-<ペインID>` で消す。
  名前付きのバッファなので、`paste-buffer`（prefix `]`）で貼り付けられる内容には影響しない。
- Claude Code のペインかどうかは、tmux の `pane_current_command` がバージョン番号（macOS）か
  `claude`（Linux、WSL を含む）かで判定している。どちらでもなくなると画面の保存が効かなくなる
  （エディタは開くが、後ろの Claude の画面が空になる）。今の値は prefix `:` → `display '#{pane_current_command}'` で確かめられる。
