# Package

version       = "0.0.0"
author        = "Zack Guard"
description   = "LC-3 implementation in Nim"
license       = "MIT"
srcDir        = "src"
bin           = @["lc3"]


# Dependencies

requires "nim >= 1.4.6"
when defined(windows):
  requires "winim"
