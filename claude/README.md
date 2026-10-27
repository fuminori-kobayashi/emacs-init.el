# Claude Code の設定

Claude Code CLI でプロンプトを入力中に `C-x C-e` を押すと、tmux の**フローティングペイン**で
`emacs -nw` が開き、編集した内容がプロンプトに戻るようにする設定。

- フローティングペインなので、マウスでリサイズできる（`display-popup` はリサイズできない）。
- Emacs を開いている間も、後ろの Claude Code のペインの表示が残る。
- 既定でエディタを開く `C-g` は、Emacs の `C-g` と紛らわしいので外している。

## 前提

- tmux 3.8 以降（`new-pane` のフローティングペインを使う）
- zsh、GNU Emacs（`emacs` が PATH にあること）

## ファイル

| ファイル | 置き場所 | 内容 |
| --- | --- | --- |
| `claude-editor` | `~/.local/bin/claude-editor` | Claude Code から呼ばれるエディタ |
| `keybindings.json` | `~/.claude/keybindings.json` | `C-g` のエディタ起動を外す |
| `../.tmux.conf_v3` | `~/.tmux.conf` | `C-x C-e` で画面を保存する bind を含む |

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

## しくみ

1. Claude Code のペインで `C-x` を押すと、tmux は `C-x` をそのまま Claude に渡し、次の 1 キーだけ
   `claude-cx` キーテーブルで受ける。`C-e` 以外のキーはふつうに処理される。
   - Claude Code のペインかどうかは、`pane_current_command` がバージョン番号（例: `2.1.311`）かどうかで判定する。
2. 続けて `C-e` が来たら、`capture-pane -e` で Claude の画面を tmux のバッファ
   `claude-screen-<ペインID>` に保存してから、`C-e` を Claude に渡す。
3. Claude Code は画面を消してから `$VISUAL`（`claude-editor`）を起動する。
4. `claude-editor` は、
   1. 保存しておいた画面を Claude のペインに書き戻し、バッファを消す。
   2. `tmux new-pane` でフローティングペイン（50%×50%、中央）を作り、その中で `emacs -nw` を開く。
   3. `new-pane` は閉じるのを待たずに戻るので、`tmux wait-for` で Emacs が終わるのを待つ。
   4. 終わったら Claude のペインにフォーカスを戻す。
5. `claude-editor` が終わると、Claude Code が編集後のファイルを読み込んでプロンプトに戻す。

tmux の外で起動したときは、ふつうに `emacs -nw` を開く。

## Claude Code の外部エディタの扱い（調べたこと）

Claude Code 2.1.311 の本体から読み取った内容。バージョンが変わると変わるかもしれない。

### どのエディタが使われるか

```js
if (VISUAL)      editor = VISUAL
else if (EDITOR) editor = EDITOR
else             editor = ["code", "vi", "nano"] のうち、PATH で最初に見つかったもの
```

- `$VISUAL` も `$EDITOR` も設定していないと、配列の中で最初に見つかったもので開く
  （VS Code があれば VS Code、なければ `vi`）。
- git の `core.editor` は見ない。
- そのため、`settings.json` の `env` で `VISUAL` / `EDITOR` を設定している。

### 画面が消える理由

Claude Code は、エディタのコマンド名で GUI エディタかどうかを判定し、端末の扱いを変える。

| エディタ | 判定 | 起動前の処理 | Claude の画面 |
| --- | --- | --- | --- |
| コマンド名に `code`、`cursor`、`windsurf`、`codium`、`subl`、`atom`、`gedit`、`notepad` を含む | GUI エディタ | `prepareTerminalForHandoff()`: マウス・フォーカスの報告を止めるだけ | 残る |
| それ以外（`claude-editor` もこちら） | 端末のエディタ | `enterAlternateScreen()`: `ESC[2J`（画面を消す）と `ESC[H` を書いてから起動 | 消える |

端末のエディタは同じ端末で全画面を使う前提なので、先に画面を空けている。
`claude-editor` は別のペインで Emacs を開くので、何もしないと消えた画面がそのまま見えてしまう。
消された画面は代替画面（alternate screen）のものなので、スクロールバックにも残らず、
エディタが起動してからでは取り戻せない。そのため、tmux で消される前に保存している。

スクリプトの名前に `gedit` などを含めて GUI エディタと判定させれば画面は消されないが、使わない。
GUI エディタと判定されると、ファイルを開くだけの場面ではエディタを切り離して起動し、終わるのを待たなくなるため。

## 注意

- Claude Code の画面の内容が、一時的に tmux のバッファに入る。ふつうは `claude-editor` がすぐ消すが、
  エディタが起動しなかった場合（プロンプト以外の場面で `C-x C-e` を押したときなど）は残り、
  `choose-buffer`（prefix `=`）で見える。気になるときは `tmux delete-buffer -b claude-screen-<ペインID>` で消す。
- 名前付きのバッファなので、`paste-buffer`（prefix `]`）で貼り付けられる内容には影響しない。
- Claude Code のプロセス名がバージョン番号でなくなると、画面の保存が効かなくなる
  （エディタは開くが、後ろの Claude の画面が空になる）。
