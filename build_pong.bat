@echo off

cargo run -- pong.gamma -e -o asm
clang -c pong.asm -o pong.o
link start_windows.o pong.o -entry:start /NODEFAULTLIB kernel32.lib SDL2.lib
