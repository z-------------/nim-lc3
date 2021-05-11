import std/os

when defined(windows):
  import ./platform/win

const
  SigInt = 2

type
  RegisterIdx = enum
    rR0 = 0
    rR1
    rR2
    rR3
    rR4
    rR5
    rR6
    rR7
    rPC
    rCond
    # rCount
  Opcode = enum
    opBr = 0 # branch
    opAdd    # add 
    opLd     # load
    opSt     # store
    opJsr    # jump register
    opAnd    # bitwise and
    opLdr    # load register
    opStr    # store register
    opRti    # unused
    opNot    # bitwise not
    opLdi    # load indirect
    opSti    # store indirect
    opJmp    # jump
    opRes    # reserved (unused)
    opLea    # load effective address
    opTrap   # execute trap
  ConditionFlag = enum
    flPos = 1 shl 0  # P
    flZro = 1 shl 1  # Z
    flNeg = 1 shl 2  # N
  MemoryMappedRegisterIdx = enum
    mrKbsr = 0xFE00  # keyboard status
    mrKbdr = 0xFE02  # keyboard data
  TrapCode = enum
    trapGetc = 0x20  # get character from keyboard, not echoed onto the terminal
    trapOut = 0x21   # output a character
    trapPuts = 0x22  # output a word string
    trapIn = 0x23    # get character from keyboard, echoed onto the terminal
    trapPutsp = 0x24 # output a byte string
    trapHalt = 0x25  # halt the program

# converters

converter uint16ToRegisterIdx(x: uint16): RegisterIdx =
  RegisterIdx(x)

converter memoryMappedRegisterIdxToUInt16(x: MemoryMappedRegisterIdx): uint16 =
  x.ord.uint16

converter conditionFlagToUInt16(x: ConditionFlag): uint16 =
  x.ord.uint16

converter intToUInt16(x: int): uint16 =
  x.uint16

# globals

var
  memory: array[uint16, uint16]
  reg: array[RegisterIdx, uint16]

# helpers

proc memWrite(address, val: uint16) =
  memory[address] = val

proc memRead(address: uint16): uint16 =
  if address == mrKbsr:
    if checkKey():
      memory[mrKbsr] = 1 shl 15
      memory[mrKbdr] = stdin.readChar().uint16
    else:
      memory[mrKbsr] = 0
  memory[address]

proc swap16(x: uint16): uint16 =
  when system.cpuEndian == littleEndian:
    (x shl 8) or (x shr 8)

proc read[T](file: File): T =
  var x: T
  discard file.readBuffer(addr x, sizeof T)
  result = x

proc readImageFile(file: File) =
  # the origin tells us where in memory to place the image
  let origin = swap16(read[uint16](file))

  # we know the maximum file size so we only need one read
  let maxRead = uint16.high - origin
  var read = file.readBuffer(addr memory[origin], maxRead * sizeof uint16)
  
  # swap to little endian
  for i in 0..<read:
    memory[origin + i] = swap16(memory[origin + i])

proc readImage(imagePath: string): bool =
  try:
    let file = open(imagePath, fmRead)
    readImageFile(file)
    file.close()
    result = true
  except IOError:
    result = false

proc signExtend(x: uint16; bitCount: int): uint16 =
  result = x
  if ((x shr (bitCount - 1)) and 0x1) > 0:
    result = result or (0xFFFF shl bitCount)

proc updateFlags(r: RegisterIdx) =
  let flag: ConditionFlag =
    if reg[r] == 0:
      flZro
    elif reg[r] shr 15 > 0: # a 1 in the leftmost bit indicates negative
      flNeg
    else:
      flPos
  reg[rCond] = flag

# signal handling
# https://gist.github.com/dom96/908782#gistcomment-2906627

proc signal*(sig: cint; fn: pointer) {.importc: "signal", header: "<signal.h>".}

template atSignal*(s: cint; actions: untyped): untyped =
  proc callback(sig: cint) =
    actions
  signal(s, callback)

# main

proc main() =
  if paramCount() < 1:
    # show usage string
    echo "lc3 [image-file1] ..."
    quit(2)
  for i in 0..<paramCount():
    let filename = paramStr(1 + i)
    if not readImage(filename):
      stderr.writeLine("failed to load image: " & filename)
      quit(1)
    
  # setup
  atSignal(SigInt):
    restoreInputBuffering();
    stdout.write('\n');
    quit(-2);
  disableInputBuffering()

  # set the PC to starting position
  # 0x3000 is the default
  let pcStart = 0x3000
  reg[rPC] = pcStart

  var running = true
  while running:
    # fetch
    let
      instr: uint16 = memRead(reg[rPC])
      op = Opcode(instr shr 12)
    inc reg[rPC]

    case op
    of opAdd:
      let
        r0: RegisterIdx = (instr shr 9) and 0x7  # dest register (DR)
        r1: RegisterIdx = (instr shr 6) and 0x7  # first operand (SR1)
        immFlag: bool = ((instr shr 5) and 0x1) != 0 # whether we are in immediate mode
      if immFlag:
        let imm5: uint16 = signExtend(instr and 0x1F, 5)
        reg[r0] = reg[r1] + imm5
      else:
        let r2: RegisterIdx = instr and 0x7
        reg[r0] = reg[r1] + reg[r2]
      updateFlags(r0)

    of opAnd:
      let
        r0: RegisterIdx = (instr shr 9) and 0x7
        r1: RegisterIdx = (instr shr 6) and 0x7
        immFlag: bool = ((instr shr 5) and 0x1) != 0
      if immFlag:
        let imm5: uint16 = signExtend(instr and 0x1F, 5)
        reg[r0] = reg[r1] and imm5
      else:
        let r2: RegisterIdx = instr and 0x7
        reg[r0] = reg[r1] and reg[r2]
      updateFlags(r0)

    of opNot:
      let
        r0: RegisterIdx = (instr shr 9) and 0x7
        r1: RegisterIdx = (instr shr 6) and 0x7
      reg[r0] = not reg[r1]
      updateFlags(r0)

    of opBr:
      let
        pcOffset: uint16 = signExtend(instr and 0x1FF, 9)
        condFlag: uint16 = (instr shr 9) and 0x7
      if (condFlag and reg[rCond]) != 0:
        reg[rPC] += pcOffset

    of opJmp: # also handles RET
      let r1: RegisterIdx = (instr shr 6) and 0x7
      reg[rPC] = reg[r1]

    of opJsr:
      let longFlag: bool = ((instr shr 11) and 0x1) != 0
      reg[rR7] = reg[rPC]
      if longFlag:
        let longPcOffset: uint16 = signExtend(instr and 0x7FF, 11)
        reg[rPC] += longPcOffset  # JSR
      else:
        let r1: RegisterIdx = (instr shr 6) and 0x7
        reg[rPC] = reg[r1]  # JSRR

    of opLd:
      let
        r0: RegisterIdx = (instr shr 9) and 0x7
        pcOffset: uint16 = signExtend(instr and 0x1FF, 9)
      reg[r0] = memRead(reg[rPC] + pcOffset)
      updateFlags(r0)

    of opLdi:
      let
        r0: RegisterIdx = (instr shr 9) and 0x7
        pcOffset: uint16 = signExtend(instr and 0x1FF, 9)
      reg[r0] = memRead(memRead(reg[rPC] + pcOffset))
      updateFlags(r0)

    of opLdr:
      let
        r0: RegisterIdx = (instr shr 9) and 0x7
        r1: RegisterIdx = (instr shr 6) and 0x7
        offset: uint16 = signExtend(instr and 0x3F, 6)
      reg[r0] = memRead(reg[r1] + offset)
      updateFlags(r0)

    of opLea:
      let
        r0: RegisterIdx = (instr shr 9) and 0x7
        pcOffset: uint16 = signExtend(instr and 0x1FF, 9)
      reg[r0] = reg[rPC] + pcOffset
      updateFlags(r0)

    of opSt:
      let
        r0: RegisterIdx = (instr shr 9) and 0x7
        pcOffset: uint16 = signExtend(instr and 0x1FF, 9)
      memWrite(reg[rPC] + pcOffset, reg[r0])

    of opSti:
      let
        r0: RegisterIdx = (instr shr 9) and 0x7
        pcOffset: uint16 = signExtend(instr and 0x1FF, 9)
      memWrite(memRead(reg[rPC] + pcOffset), reg[r0])

    of opStr:
      let
        r0: RegisterIdx = (instr shr 9) and 0x7
        r1: RegisterIdx = (instr shr 6) and 0x7
        offset: uint16 = signExtend(instr and 0x3F, 6)
      memWrite(reg[r1] + offset, reg[r0])

    of opTrap:
      let trapCode = Trapcode(instr and 0xFF)

      case trapCode
      of trapGetc:
        reg[rR0] = stdin.readChar().uint16

      of trapOut:
        stdout.write(reg[rR0].char)
        stdout.flushFile()

      of trapPuts:
        # one char per word
        var p = reg[rR0]
        while memory[p] != 0:
          stdout.write(memory[p].char)
          inc p
        stdout.flushFile()

      of trapIn:
        stdout.writeLine("Enter a character: ")
        let c = stdin.readChar()
        stdout.write(c)
        reg[rR0] = c.uint16

      of trapPutsp:
        # one char per byte (two bytes per word)
        # here we need to swap back to big endian
        var p = reg[rR0]
        while memory[p] != 0:
          let
            char1 = (memory[p] and 0xFF).char
            char2 = (memory[p] shr 8).char
          stdout.write(char1)
          if char2 != '\0':
            stdout.write(char2)
          inc p
        stdout.flushFile()

      of trapHalt:
        stdout.write("HALT")
        stdout.flushFile()
        running = false

    else:
      raise newException(ValueError, "Invalid opcode " & $op)
        
  # shutdown
  restoreInputBuffering()


when isMainModule:
  main()
