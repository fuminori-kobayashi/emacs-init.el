# MSYS2 で使う

Windows Terminal で MSYS2 の bash を開き、その中で tmux と Claude Code（Windows 版）を動かす前提。

## tmux・Emacs

- tmux と Emacs は pacman で入れる（`pacman -S tmux emacs`）。パッケージ一覧が古いと tmux も古い版になるので、先に `pacman -Syu` で全体を更新する。
- Windows Terminal (ConPTY) で tmux を直接動かすと、極端に遅くなりキー入力もおかしくなる。
  `~/.bashrc` で `tmux` を関数にし、tmux の外から端末で起動するときは `script` を通して MSYS2 の pty の中で動かす。

  ```sh
  tmux() {
    if [[ -n $TMUX || ! -t 0 || ! -t 1 ]]; then
      command tmux "$@"
      return
    fi
    local args=
    (( $# )) && printf -v args ' %q' "$@"
    script -qec "tmux$args" /dev/null
  }
  ```

- ripgrep は ucrt64 のパッケージしかない（`pacman -S mingw-w64-ucrt-x86_64-ripgrep`）。
  `~/.bashrc` で PATH の**後ろ**に `/ucrt64/bin` を足す（MSYS2 と同じ名前のコマンドは MSYS2 のものを使うため）。
- `.tmux.conf` は MSYS2 では、コピーを `/dev/clipboard` に書き（`clip.exe` は日本語が化ける）、`escape-time` を 100ms にする
  （10ms だとマウスのエスケープシーケンスを読み切れず、`MMMM...` がペインに入力される）。

## Betterleaks

Homebrew がないので、[GitHub のリリース](https://github.com/betterleaks/betterleaks/releases)から
`betterleaks_<版>_windows_x64.zip` を取り、`checksums.txt` と照らし合わせてから、中の `betterleaks.exe` を PATH の通った場所に置く。
pre-commit フックの自動アップデートは働かないので、新しい版が出たら同じように入れ直す。

## Claude Code

- 設定の置き場所は MSYS2 の HOME ではなく `%USERPROFILE%\.claude`。
- Bash ツールは既定では Git for Windows の bash で動き、MSYS2 の tmux などが見えない。
  `settings.json` の `env` に `"CLAUDE_CODE_GIT_BASH_PATH": "C:\\msys64\\usr\\bin\\bash.exe"` を足す。

### プロンプトを Emacs で編集する

[claude/README](claude/README.md) の設定を MSYS2 で使うときの違い。
Windows 版の Claude Code は `VISUAL` を cmd.exe で起動するので、bash スクリプトを直接は指定できない。
`claude-notepad.cmd` から MSYS2 の bash で `claude-notepad` を呼ぶ。

```sh
cp claude/claude-notepad claude/claude-notepad.cmd ~/.local/bin/
chmod +x ~/.local/bin/claude-notepad
cp claude/keybindings.json "$USERPROFILE/.claude/keybindings.json"   # 既にある場合は中身をマージする
```

`settings.json` の `env` には、`claude-notepad.cmd` を Windows 形式の絶対パスで書く。

```json
"env": {
  "VISUAL": "C:\\msys64\\home\\<ユーザー名>\\.local\\bin\\claude-notepad.cmd",
  "EDITOR": "C:\\msys64\\home\\<ユーザー名>\\.local\\bin\\claude-notepad.cmd"
}
```

- MSYS2 を `C:\msys64` 以外に入れている場合は、`claude-notepad.cmd` の中の `bash.exe` のパスを直す。
- Claude Code はファイルのパスや `TEMP` などを Windows 形式（`C:\...`）で渡すので、`claude-notepad` が `cygpath` で MSYS2 の形式に直してから Emacs に渡す。
- `claude-notepad.cmd` のコメントは英語にしている。cmd.exe はバッチファイルを UTF-8 ではなく ANSI コードページで読むので、日本語を書くとコマンドとして実行されてしまう。
