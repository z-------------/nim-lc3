import winim/lean

var
  hStdin: HANDLE = INVALID_HANDLE_VALUE
  fdwMode: DWORD
  fdwOldMode: DWORD

proc kbHit(): cint {.header: "<conio.h>", importc: "_kbhit".}

proc checkKey*(): bool =
  let
    a = WaitForSingleObject(hStdin, 1000) == WAIT_OBJECT_0
    b = kbHit() != 0
  # dump a
  # dump b
  a and b

proc disableInputBuffering*() =
  hStdin = GetStdHandle(STD_INPUT_HANDLE)
  GetConsoleMode(hStdin, addr fdwOldMode)  # save old mode
  fdwMode = fdwOldMode xor
            ENABLE_ECHO_INPUT xor  # no input echo
            ENABLE_LINE_INPUT      # return when one or more charaters are available
  SetConsoleMode(hStdin, fdwMode)  # set new mode
  FlushConsoleInputBuffer(hStdin)  # clear buffer

proc restoreInputBuffering*() =
  SetConsoleMode(hStdin, fdwOldMode)
