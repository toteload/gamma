@echo off

clang-cl -c start_windows.c -o start_windows.o -EHa- -nologo -GR- -GS- -Gs99999999

cargo run -- pong.gamma

clang -Xlinker -nodefaultlib ^
      -Xlinker -nologo ^
      -Xlinker -subsystem:windows ^
      -o pong.exe ^
      pong.o start_windows.o -lSDL2 -lkernel32 -lmsvcrt

