@echo off

cargo run -- pong.gamma -e -o asm -t windows
clang -c pong.s -o pong.o
link start_windows.o pong.o /ENTRY:start /NODEFAULTLIB kernel32.lib user32.lib SDL2.lib winmm.lib gdi32.lib ole32.lib shell32.lib setupapi.lib advapi32.lib version.lib imm32.lib oleaut32.lib uuid.lib /OUT:pong.exe
