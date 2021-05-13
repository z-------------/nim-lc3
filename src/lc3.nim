import std/os
import std/macros

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

template whenOp(mask: untyped; body: untyped): untyped =
  if (mask and opBit) != 0:
    stmtList.add quote do:
      body

macro ins(op: static[Opcode]): untyped =
  result = nnkLambda.newTree(
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(
      newEmptyNode(), # proc return type
      nnkIdentDefs.newTree(
        newIdentNode("instr"),
        newIdentNode("uint16"),
        newEmptyNode()  # default value
      )
    ),
    newEmptyNode(),
    newEmptyNode(),
  )
  var stmtList = newStmtList()

  let opBit: uint16 = 1 shl op.ord

  whenOp(0x4EEE):
    var r0 {.inject.}: RegisterIdx
  whenOp(0x12F3):
    var r1 {.inject.}: RegisterIdx
  whenOp(0x0022):
    var r2 {.inject.}: RegisterIdx
  whenOp(0x0022):
    var
      imm5 {.inject.}: uint16
      immFlag {.inject.}: uint16
  whenOp(0x4C1D):
    var pcPlusOff {.inject.}: uint16
  whenOp(0x00C0):
    var basePlusOff {.inject.}: uint16
  
  whenOp(0x4EEE):
    r0 = (instr shr 9) and 0x7
  whenOp(0x12F3):
    r1 = (instr shr 6) and 0x7
  whenOp(0x0022):
    immFlag = (instr shr 5) and 0x1
    if immFlag != 0:
      imm5 = signExtend(instr and 0x1F, 5)
    else:
      r2 = instr and 0x7
  whenOp(0x00C0): # base + offset
    basePlusOff = reg[r1] + signExtend(instr and 0x3F, 6)
  whenOp(0x4C0D): # indirect address
    pcPlusOff = reg[rPC] + signExtend(instr and 0x1FF, 9)
  whenOp(0x0001): # BR
    let cond: uint16 = (instr shr 9) and 0x7
    if (cond and reg[rCond]) != 0:
      reg[rPC] = pcPlusOff
  whenOp(0x0002): # ADD
    if immFlag != 0:
      reg[r0] = reg[r1] + imm5
    else:
      reg[r0] = reg[r1] + reg[r2]
  whenOp(0x0020): # AND
    if immFlag != 0:
      reg[r0] = reg[r1] and imm5
    else:
      reg[r0] = reg[r1] and reg[r2]
  whenOp(0x0200): # NOT
    reg[r0] = not reg[r1]
  whenOp(0x1000): # JMP
    reg[rPC] = reg[r1]
  whenOp(0x0010): # JSR
    let longFlag: uint16 = (instr shr 11) and 0x1
    reg[rR7] = reg[rPC]
    if longFlag != 0:
      pcPlusOff = reg[rPC] + signExtend(instr and 0x7FF, 11)
      reg[rPC] = pcPlusOff
    else:
      reg[rPC] = reg[r1]
  whenOp(0x0004): # LD
    reg[r0] = memRead(pcPlusOff)
  whenOp(0x0400): # LDI
    reg[r0] = memRead(memRead(pcPlusOff))
  whenOp(0x0040): # LDR
    reg[r0] = memRead(basePlusOff)
  whenOp(0x4000): # LEA
    reg[r0] = pcPlusOff
  whenOp(0x0008): # ST
    memWrite(pcPlusOff, reg[r0])
  whenOp(0x0800): # STI
    memWrite(memRead(pcPlusOff), reg[r0])
  whenOp(0x0080): # STR
    memWrite(basePlusOff, reg[r0])
  whenOp(0x8000): # TRAP
    handleTrap(instr)
  # whenOp(0x0100): # RTI
  #   discard
  whenOp(0x4666):
    updateFlags(r0)
  
  result.add(stmtList)

const opTable = [
  ins(opBr),
  ins(opAdd),
  ins(opLd),
  ins(opSt),
  ins(opJsr),
  ins(opAnd),
  ins(opLdr),
  ins(opStr),
  ins(opRti),
  ins(opNot),
  ins(opLdi),
  ins(opSti),
  ins(opJmp),
  ins(opRes),
  ins(opLea),
  ins(opTrap),
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

    opTable[op.ord](instr)
        
  # shutdown
  restoreInputBuffering()


when isMainModule:
  main()
