# 個人用設定ファイル

macOS、Linux、WSL (Ubuntu) で使う。

| パス | 置き場所 | 内容 |
| --- | --- | --- |
| `.emacs.d/init.el` | `~/.emacs.d/init.el` | Emacs の設定 |
| `.tmux.conf` | `~/.tmux.conf` | tmux の設定 |
| `claude/` | `~/.local/bin/`、`~/.claude/` | Claude Code のプロンプトを Emacs で編集する設定（[README](claude/README.md)） |
| `javascript/` | 各プロジェクトのルート | ESLint / Prettier の設定の雛形（[README](javascript/README.md)） |

## 前提

- Emacs 27.2 以降。MELPA のパッケージ（`web-mode`、`flycheck`、`prettier-js`、`company`、`copilot` など）と ripgrep が要る。
- tmux 3.2 以降。prefix は `C-j`。Linux (X11) ではマウス選択がクリップボードにコピーされない。
- Betterleaks（pre-commit フックで使う）。`brew install betterleaks` で入れる。
  Linux / WSL は [Homebrew on Linux](https://docs.brew.sh/Homebrew-on-Linux) を入れ、`brew shellenv` の行を `~/.bashrc` などに足して PATH を通しておく。

API キーなどの秘密情報は `.env` や `*.local` に分ける（`.gitignore` で除外している）。
clone したら `git config core.hooksPath githooks` を実行し、コミット時に Betterleaks で機密情報を調べる pre-commit フックを有効にする。
