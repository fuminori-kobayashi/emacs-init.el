# ESLint / Prettier の設定

ESLint と Prettier の設定の雛形。ホームに置いても効かないので、プロジェクトのルートにコピーして使う。

## ファイル

| ファイル | 内容 |
| --- | --- |
| `eslint.config.mjs` | ESLint 9 以降の設定（flat config） |
| `.eslintrc.json` | ESLint 7、8 の設定 |
| `.prettierrc.json` | Prettier の設定 |

## 前提

ESLint 7 以降で使える。プロジェクトの ESLint のバージョンに合わせて、どちらか一方をコピーする。

| ESLint | コピーするファイル | 要るパッケージ |
| --- | --- | --- |
| 9 以降 | `eslint.config.mjs` | `npm i -D eslint @eslint/js globals @stylistic/eslint-plugin` |
| 7、8 | `.eslintrc.json` | `eslint` だけ（プロジェクトに入っているものをそのまま使う） |

Prettier は `npm i -D prettier` で入れる。

## 注意

- ルールは2つの ESLint の設定で同じ。
- ES Modules ではなくスクリプトとして解析する（`no-implicit-globals` を効かせるため）。
  `import` を使うプロジェクトでは、`eslint.config.mjs` は `sourceType` を `'module'` に、
  `.eslintrc.json` は `"parserOptions": { "sourceType": "module" }` を足す。
- 解析できる構文が違う。`eslint.config.mjs` は最新の構文まで、`.eslintrc.json` は ES2015（`env` の `es6`）まで。
  `.eslintrc.json` で `async` / `await` などを使うなら、`env` の `es6` を `es2021` などに変える（ESLint 7.8 以降）。
- 書式のルール（`no-multi-spaces` など）は ESLint 本体では 8.53 から非推奨になった。
  `eslint.config.mjs` では `@stylistic` のものを使っている。`.eslintrc.json` は ESLint 本体のものをそのまま使う
  （非推奨だが、ESLint 8 でも動く）。
- ESLint 9 は `ESLINT_USE_FLAT_CONFIG=false` を付ければ `.eslintrc.json` も読めるが、ESLint 10 では読めない。
  9 以降は `eslint.config.mjs` を使う。
