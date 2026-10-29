@echo off
rem Launch claude-notepad (a bash script) from the Windows build of Claude Code on MSYS2.
rem Claude Code runs VISUAL through cmd.exe on Windows, so a bash script cannot be set directly.
rem Keep "notepad" in the name so Claude Code treats it as a GUI editor (same as claude-notepad).
rem Edit the bash.exe path if MSYS2 is not installed in C:\msys64.
rem (Comments are in English because cmd.exe reads batch files in the ANSI code page, not UTF-8.)
"C:\msys64\usr\bin\bash.exe" "%~dp0claude-notepad" %*
exit /b %errorlevel%
