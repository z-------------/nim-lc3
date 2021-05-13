import std/os
from std/strutils import toHex

when defined(windows):
  import ./platform/win
elif defined(posix):
  import ./platform/unix

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
  running = true

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

proc handleTrap(instr: uint16) =
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

# instruction macro

template ifOp(ops: varargs[Opcode]; body: untyped): untyped =
  if static(ops.contains(op)):
    body

proc ins[op: static[Opcode]](instr: uint16) =
  var
    r0, r1, r2: RegisterIdx
    imm5, immFlag: uint16
    pcPlusOff, basePlusOff: uint16
  
  ifOp(opAdd, opAnd, opNot, opLd, opLdi, opLdr, opLea, opSt, opSti, opStr):
    r0 = (instr shr 9) and 0x7
  ifOp(opAdd, opAnd, opNot, opJmp, opJsr, opLdr, opStr):
    r1 = (instr shr 6) and 0x7
  ifOp(opAdd, opAnd):
    immFlag = (instr shr 5) and 0x1
    if immFlag != 0:
      imm5 = signExtend(instr and 0x1F, 5)
    else:
      r2 = instr and 0x7
  ifOp(opLdr, opStr): # base + offset
    basePlusOff = reg[r1] + signExtend(instr and 0x3F, 6)
  ifOp(opBr, opJsr, opLd, opLdi, opLea, opSt, opSti): # indirect address
    pcPlusOff = reg[rPC] + signExtend(instr and 0x1FF, 9)
  ifOp(opBr): # BR
    let cond: uint16 = (instr shr 9) and 0x7
    if (cond and reg[rCond]) != 0:
      reg[rPC] = pcPlusOff
  ifOp(opAdd): # ADD
    if immFlag != 0:
      reg[r0] = reg[r1] + imm5
    else:
      reg[r0] = reg[r1] + reg[r2]
  ifOp(opAnd): # AND
    if immFlag != 0:
      reg[r0] = reg[r1] and imm5
    else:
      reg[r0] = reg[r1] and reg[r2]
  ifOp(opNot): # NOT
    reg[r0] = not reg[r1]
  ifOp(opJmp): # JMP
    reg[rPC] = reg[r1]
  ifOp(opJsr): # JSR
    let longFlag: uint16 = (instr shr 11) and 0x1
    reg[rR7] = reg[rPC]
    if longFlag != 0:
      pcPlusOff = reg[rPC] + signExtend(instr and 0x7FF, 11)
      reg[rPC] = pcPlusOff
    else:
      reg[rPC] = reg[r1]
  ifOp(opLd): # LD
    reg[r0] = memRead(pcPlusOff)
  ifOp(opLdi): # LDI
    reg[r0] = memRead(memRead(pcPlusOff))
  ifOp(opLdr): # LDR
    reg[r0] = memRead(basePlusOff)
  ifOp(opLea): # LEA
    reg[r0] = pcPlusOff
  ifOp(opSt): # ST
    memWrite(pcPlusOff, reg[r0])
  ifOp(opSti): # STI
    memWrite(memRead(pcPlusOff), reg[r0])
  ifOp(opStr): # STR
    memWrite(basePlusOff, reg[r0])
  ifOp(opTrap): # TRAP
    handleTrap(instr)
  # ifOp(0x0100): # RTI
  #   discard
  ifOp(opAdd, opAnd, opNot, opLd, opLdi, opLdr, opLea):
    updateFlags(r0)

const opTable = [
  ins[opBr],
  ins[opAdd],
  ins[opLd],
  ins[opSt],
  ins[opJsr],
  ins[opAnd],
  ins[opLdr],
  ins[opStr],
  ins[opRti],
  ins[opNot],
  ins[opLdi],
  ins[opSti],
  ins[opJmp],
  ins[opRes],
  ins[opLea],
  ins[opTrap],
]

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
  setControlCHook():
    restoreInputBuffering();
    stdout.write('\n');
    quit(-2);
  disableInputBuffering()

  # set the PC to starting position
  # 0x3000 is the default
  let pcStart = 0x3000
  reg[rPC] = pcStart

  while running:
    # fetch
    let
      instr: uint16 = memRead(reg[rPC])
      op = Opcode(instr shr 12)
    inc reg[rPC]

    try:
      opTable[op.ord](instr)
    except:
      stderr.writeLine("PC = " & $reg[rPC].toHex)
      raise
        
  # shutdown
  restoreInputBuffering()


when isMainModule:
  main()
