import std/posix  # thank fuck
import std/termios  # god is real

var
  originalTio: Termios

proc checkKey*(): bool =
  var readFds: TFdSet
  FD_ZERO(readFds)
  FD_SET(STDIN_FILENO, readFds)

  var timeout: Timeval
  timeout.tvSec = 0.Time
  timeout.tvUsec = 0.clong
  result = select(1, addr readFds, nil, nil, addr timeout) != 0

proc disableInputBuffering*() =
  discard tcgetattr(STDIN_FILENO, addr originalTio)
  var newTio: Termios = originalTio
  newTio.cLflag = newTio.cLflag and not ICANON and not ECHO
  discard tcsetattr(STDIN_FILENO, TCSANOW, addr newTio)

proc restoreInputBuffering*() =
  discard tcsetattr(STDIN_FILENO, TCSANOW, addr originalTio)
