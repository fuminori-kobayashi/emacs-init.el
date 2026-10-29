// ESLint 9 以降の flat config
// 要: npm i -D eslint @eslint/js globals @stylistic/eslint-plugin
import js from '@eslint/js';
import globals from 'globals';
import stylistic from '@stylistic/eslint-plugin';

export default [
    js.configs.recommended,
    {
        languageOptions: {
            ecmaVersion: 'latest',
            // no-implicit-globals を効かせるため、ES Modules ではなくスクリプトとして解析する
            sourceType: 'script',
            globals: globals.browser,
        },
        plugins: {
            '@stylistic': stylistic,
        },
        rules: {
            'no-multi-str': 'error',
            eqeqeq: ['error', 'always'],
            'no-implicit-globals': 'error',
            'no-throw-literal': 'error',
            radix: 'error',
            'no-undefined': 'error',
            // 書式のルールは ESLint 本体では非推奨になったので @stylistic のものを使う
            '@stylistic/no-multi-spaces': 'error',
            '@stylistic/dot-location': ['error', 'property'],
            '@stylistic/semi-spacing': ['error', { before: false, after: true }],
            '@stylistic/spaced-comment': ['error', 'always'],
            '@stylistic/space-infix-ops': ['error', { int32Hint: false }],
        },
    },
];
