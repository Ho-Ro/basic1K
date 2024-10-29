; Will Stevens
; 3rd April 2024
; 1K Z80 BASIC
; GPL v3
;
; Terminal settings:
;
; Assumes that outputting a newline requires
; CR and LF, and that pressing return on the
; terminal sends CR alone. 1K BASIC echoes all
; characters it receives back to the terminal.
; I believe that these settings are compatible
; with using a Teletype Model 33 in full duplex
; mode. If using a VT100 terminal emulator, LNM
; must be reset so that pressing Return sends
; CR alone.
;
; Development log:
; 2024-04-03 Started from 8080 version
; 2024-04-10 Initial port. 24 bytes free.
;            Need to explore potentia of
;            using LDIR/LDDR for program
;            editing. Untested, not expected
;            to work yet.
; 2024-04-12 Changed I/O to support Altair32
;            emulator, which can use a Z80
; 2024-04-17 Several bug fixes. Progressing
;            towards a working version
; 2024-04-20 Fixed introduced bug where byte
;            before TolenList did not have high
;            bit set.
; 2024-04-21 Added PEEK function
; 2024-04-24 Put in faster DivSub. 15 bytes left
; 1024-05-12 Fixed bug in PrintInteger. 17 bytes
;            free.

; For development purposes assume we have
; 1K ROM from 0000h-03FFh containing BASIC
; 1K RAM from 0400h-0800h

RAM_BASE            .equ    0400h
RAM_TOP             .equ    0800h               ; 1 more than top byte of RAM

; Communication interface is 6850 ACIA
IODATA              .equ    01h                 ; data register address
IOSTAT              .equ    02h                 ; status register addres
RDRF                .equ    01h                 ; receive data register full mask
TDRE                .equ    02h                 ; transmit data register empty mask
TX_ALWAYS_READY     .equ    1                   ; simulated 6850, TX is always ready

LF                  .equ    0AH
CR                  .equ    0DH

; Token values
; 0-31 are variables (0 = @)

; IntegerToken must be one more than last var
IntegerToken        .equ    32                  ; followed by 16-bit integer
QuestionMarkToken   .equ    33                  ; indicates syntax error
StringToken         .equ    34                  ; followed by string, followed by end quote

; Callable tokens are low byte of subroutine to call

; Errors are displayed as Ex where x is an error
; code which is tbe address on the stack when
; Error subroutine is called.

; Input buffer is just 8 bytes long
; used by input statement to get an integer.
; If there is a buffer overflow because user
; enters too much, the behaviour is system
; dependent - e.g. if writes above RAM
; space do nothing then its not a problem.
; If memory space repeats and lower 1K
; is ROM then also not much of a problem.

INPUT_BUFFER        .equ    RAM_TOP-8
STACK_INIT          .equ    RAM_TOP-8

; this must be on a 256 byte boundary
VAR_SPACE           .equ    RAM_BASE

; 30 words, first of which is not
; accessible to user, so can be
; used for PROG_PTR
PROG_PTR            .equ    RAM_BASE

; 2 words accessible to user as variables
; 30 and 31 (^ and _)
PROG_PARSE_PTR      .equ    RAM_BASE+60
RNG_SEED            .equ    RAM_BASE+62

PROG_BASE           .equ    RAM_BASE+64


                    .org    00h

                    LD      HL,PROG_BASE
                    LD      (PROG_PTR),HL
                    JR      ReadyJump

                    .org    08h

PutChar:
; PutChar is called frequently
; PutChar must return with Z set
                    OUT     (IODATA),A          ; 6850 port 1 is for char I/O

                                                ; Having the wait loop after the character
                                                ; is output will slow down I/O when running
                                                ; on hardware, but I can't think of a way
                                                ; of fitting this into 8 bytes otherwise.

PutCharWaitLoop:
                                                ; address 000ah
                                                ; TODO change these few instructions
                                                ; if targetting hardware
                    .IF     TX_ALWAYS_READY     ; simulated ACIA
                    XOR     A                   ; do not check for ready
                    RET
                    .ELSE                       ; real ACIA HW
                    IN      A,(IOSTAT)
                    AND     TDRE                ; char transmitted?
                    RET     NZ                  ; yes, ready
                    .db     0c3h                ; opcode for JP nnnn
                                                ; the following two bytes are
                                                ; 0ah and 00h, so this jumps to
                                                ; PutCharWaitLoop
                    .ENDIF

                    .org    10h

LDAXB_INXB_CPI:
                    LD      A,(BC)              ; must be opcode 0ah
                    NOP                         ; must be opcode 00h
                    INC     BC
                    EX      (SP),HL
                    CP      (HL)
CompareJump_Skip:
                    INC     HL
ExpApplyOp:                                     ; shared code
                    EX      (SP),HL
                    RET


CP_BC               .macro  value
                    RST     LDAXB_INXB_CPI
                    .db     value
                    .endm


                    .org    18h

CompareJump:
; byte after RST is compared with A
; if equal then jump to address on same page.
;
; only use where performance is not
; important (parsing, printing)

                    EX      (SP),HL
                    CP      (HL)
                    INC     HL
                    JR      NZ,CompareJump_Skip
                    LD      L,(HL)
                    JR      CompareJump_Skip

; RST 4 and 8 bytes free

CP_JP_Z             .macro  value,dest
                    RST     CompareJump
                    .db     value & 0FFH
                    .db     (dest & 0FFH) - 1
                    .endm


                    .org    28h

CompareHLDE:
; compare HL and DE, return
; Z equal, NZ if not equal
; C equal, NC if not equal
; A will be zero if Z is set
                    LD      A,L
                    XOR     E
                    RET     NZ
                    LD      A,H
                    XOR     D
                    RET     NZ
                    SCF
                    RET


                    .org    30h

NegateDE:
                                                ; flags are not affected
                                                ; decrement and invert so that we end
                                                ; up with D in A - sometimes handy
                    DEC     DE
                    LD      A,E
                    CPL
                    LD      E,A
                    LD      A,D
                    CPL
                    LD      D,A
                    RET


                    .org    38h

ExpEvaluate:
; BC points to program
; DE contains value
; Stack is used for both operands and
; operators

; ExpEvaluate must not be called
; from page 1, The hi byte of the return address
; is a marker to distinguish it from an operator.
; Only operators have hi byte = 1

ExpEvaluateNum:
                                                ; Expecting ( var integer or - sign
                                                ; or function call
                    CP_BC   LeftBraceToken
                    JR      Z,ExpLeftBrace
                    CP_JP_Z SubSub,ExpNegate
                                                ; last function
                    CP      (RndSub+1)&0ffh
                    JR      NC,Error
                                                ; first function
                    CP      AbsSub&0ffh
                    JR      NC,FunctionCall     ; between RndSub and AbsSub

                    CP      IntegerToken
                                                ; Integer token is one more than last var
                                                ; token so if carry is set then it is a var
                    JR      C,ExpVar

                    JR      NZ,Error

                                                ; Fall through to ExpInteger
ExpInteger:
                    LD      H,B
                    LD      L,C
                    INC     BC
                    INC     BC

                                                ; fall through with carry clear
ExpVar:
                                                ; carry set if jumped to here

                    CALL    C,GetVarLocation
ExpVarGetValue:
                    LD      E,(HL)
                    INC     HL
                    LD      D,(HL)

ExpEvaluateOp:
                                                ; Expecting operator or right bracket or
                                                ; end of expression

                                                ; Are there operators on the stack?
                    POP     HL

                                                ; H will be 0, 2 or 3 if no operators on
                                                ; stack (i.e. high byte of return address)

                    LD      A,H
                    DEC     A
                    JR      NZ,SkipExpApplyOp

                                                ; if L is equal to MulSub then apply it.
                                                ; this gives * same precedence as /
                                                ; MOV A,L
                                                ; RST CompareJump
                                                ; DB (MulSub&0ffh),(ExpApplyOp&0ffh)-1

                    LD      A,L
                    CP      NegateSub&0ffh
                    JR      NC,ExpApplyOp

                    LD      A,(BC)

                                                ; No longer needed since case below
                                                ; includes this
                                                ; CPI Operators&0ffh
                                                ; Is it the end of the expression?
                                                ; JC ExpApplyOp

                                                ; Does operator on stack have GTE precedence?
                                                ; (or end of expression, when A < operators)
                    DEC     A
                    CP      L

                    JR      C,ExpApplyOp        ; apply the operator
                                                ; that was on the stack

SkipExpApplyOp:
                    PUSH    HL                  ; put operator that was on stack
                                                ; back onto stack

                    LD      A,(BC)

                    CP      Operators&0ffh
                                                ; Is it the end of the expression?
                    RET     C

                    INC     BC

                                                ; fall through
                    .db     21h                 ; LH HL,nnnn eats 2 bytes
ExpNegate:
                    INC     A
                    LD      DE,0

                    LD      HL,ExpEvaluateOp    ; address to return to
                                                ; after operator is called
                    PUSH    HL

                                                ; Push operand and operator onto the stack

                    PUSH    DE                  ; operand

                    LD      L,A
                    INC     H                   ; Assumes H was 0 and needs to be 1
                    PUSH    HL

                    JR      ExpEvaluateNum

ReadyJump:
                    JR      Ready

ForSubImpl:
                                                ; Stack contains return address:
                                                ; ExecuteProgramLoop - EPL
                                                ; Keep it there even though it isn't used by
                                                ; ForSub, it will be used by NextSub

                    PUSH    HL                  ; stack contains <SP> VL+1, EPL

                                                ; check that we have a 'TO' token
                    CP_BC   ToToken
                    CALL    NZ,Error

                    RST     ExpEvaluate
                    RST     NegateDE

                    PUSH    DE                  ; stack contains <SP> -T,VL+1, EPL
                                                ; T is target

                                                ; step is going to be 1 unless we encounter
                                                ; a STEP token
                    LD      DE,1
                    LD      A,(BC)

                                                ; check for optional STEP token
                    CP_JP_Z StepToken,ForWithStep

                    .db     21h                 ; LD HL,nnnn opcode eats the next 2 bytes
ForWithStep:
                                                ; we have step token
                    INC     BC
                    RST     ExpEvaluate

                                                ; TODO on Z80 we can do POP AF, then
                                                ; push it back belown instead of DEC SP*2
                                                ; to save a byte
                    POP     HL
                    POP     HL                  ; H contains VL+1

                                                ; B contains the start address of the
                                                ; loop (LS)

                    PUSH    BC                  ; stack contains -T <SP> LS,EPL
                    DEC     SP
                    DEC     SP                  ; stack contains <SP> -T,LS,EPL
                    PUSH    DE                  ; stack contains <SP>,S,-T,LS,EPL
                    PUSH    HL                  ; stack contains <SP>,VL+1,S,-T,LS,EPL

                    JP      ExecuteProgramLoop

ExpLeftBrace:
                    DEC     BC
FunctionCall:
                                                ; push return address
                    LD      HL,ExpEvaluateOp
                    PUSH    HL
                                                ; A contains the address to call on page 1
                                                ; push function address
                    LD      L,A
                    INC     H                   ; Assumes H was 0 and is now 1
                    PUSH    HL

                                                ; fall through

                                                ; This must be before Error so that it
                                                ; can fall through
ExpBracketedB:
                    CP_BC   LeftBraceToken
                    CALL    NZ,Error

                    RST     ExpEvaluate

                    CP_BC   RightBraceToken
                    RET     Z

                                                ; fall through


; Display error code and go back to line entry
Error:
                    CALL    CRLF
                    LD      A,'E'
                    RST     PutChar
                    POP     DE
                    CALL    PrintInteger

                                                ; fall through

                                                ; we need ready to be at an address
                                                ; corresponding to harmless opcodes when
                                                ; executed in ExecuteProgram
                    org     00bbh
Ready:
                                                ; Set stack pointer
                                                ; Do this every time to guard against
                                                ; GOSUB with no RETURN errors

                    LD      SP,STACK_INIT

                    CALL    CRLF

                                                ; Use this if no CRLF is needed
                                                ; and sure that stack can't be wrong
ReadyNoNewline:

                    LD      HL,(PROG_PTR)
                    PUSH    HL                  ; push it because we need it after
                                                ; GetLine

                    CALL    GetLine

                    LD      (HL),EndProgram&0ffh

                    LD      (PROG_PARSE_PTR),HL
                    POP     HL

                    PUSH    HL
                    POP     BC

                    LD      A,(HL)
                                                ; Regardless of which branch taken
                                                ; we need this marker here.
                                                ; This overwrites the token to execute,
                                                ; but we've already got that in A
                    LD      (HL),EndProgram&0ffh

                    CP      IntegerToken
                    JP      NZ,ExecuteDirect

LineStartsWithInt:
                                                ; Get the line number into DE
                    INC     HL
                    LD      E,(HL)
                    INC     HL
                    LD      D,(HL)
                    INC     HL

                                                ; Is it an integer all by itself?
                                                ; If so then delete the line

                                                ; call GetLineNum to find either the line, or
                                                ; pointer to next location in program after it

                    LD      H,(HL)              ; preserve M (GetLineNum doesn't touch H)
                    CALL    GetLineNum
                    LD      A,H
                    LD      HL,(PROG_PTR)
                    PUSH    AF

                    CP_JP_Z EndProgram,DeleteProgramLine

                    POP     AF
                                                ; if GetLineNum returns a match then this is
                                                ; an error, user must delete line first
                    CALL    Z,Error

                                                ; do a memory rotate with
                                                ; first = GetLine/ATNLN address
                                                ; middle = PROG_PTR
                                                ; last = PROG_PARSE_PTR

                    LD      (HL),LineNumSub&0ffh; undo what we did earlier
                    EX      DE,HL
                    LD      HL,(PROG_PARSE_PTR)

                    PUSH    HL                  ; last
                    PUSH    BC                  ; first

                    PUSH    DE                  ; middle

                                                ; carry is clear here from the call to
                                                ; GetLineNum

                    JR      Entry

DeleteProgramLine:
; 25 bytes
                    POP     AF

                    JR      NZ,ReadyNoNewLine   ; line not found, do nothing

                    PUSH    HL
                    PUSH    BC                  ; first
                    PUSH    HL                  ; last

                    ADD     HL,BC               ; HL=PROG_PTR+first

                    INC     BC
                    CALL    ATNLN_Int           ; Z is set when this is called

                                                ; set HL to what we want PROG_PTR to be
                    LD      D,B
                    LD      E,C
                    RST     NegateDE

                    ADD     HL,DE               ; HL=PROG_PTR+first-middle

                                                ; because DAD D above always causes HL
                                                ; to decrease, it must set carry
                                                ; so STC below is not needed

                                                ; STC ; skip first reverse in memory rotate
                                                ; because we don't care about the
                                                ; line being deleted

Entry:
                                                ; carry is clear if coming from insert

                    PUSH    BC                  ; middle (or first)

                    LD      (PROG_PTR),HL

MemoryRotate:
; 27 bytes
; stack must contain (from top down)
; first, middle, first, last
; DE = middle
; HL = Last

                    CALL    NC,Reverse
                    CALL    ReverseDH

                    LD      BC,ReadyNoNewLine
                    PUSH    BC

ReverseDH:
                    POP     HL
                    POP     DE
                    EX      (SP),HL

Reverse:
; HL = last (i.e 1 after the last byte to swap)
; DE = first

ReverseLoop:
                    RST     CompareHLDE
                    RET     Z
                    DEC     HL
                    RST     CompareHLDE
                    RET     Z

                    LD      B,(HL)
                    LD      A,(DE)
                    LD      (HL),A
                    LD      A,B
                    LD      (DE),A
                    INC     DE

                    JR      ReverseLoop


; GetLine sits entirely in page 1
; good - it uses RST CompareJump in two
; places, so be careful if moving it
; Also it assumes ClassLookup on same page
; as NoCharClass

NLTestTrue:
                                                ; error if we are in the middle
                                                ; of a string
                    LD      A,L
                    CP      QuoteClassExpEnd&0ffh
                    CALL    Z,Error

                    POP     HL

                    JP      CRLF

GetLine:
                                                ; HL points where we want the line to be
                                                ; parsed to.
                                                ; On return HL points to byte after what we've
                                                ; got.

                    LD      A,'>'
                    RST     PutChar

GetLineNoPrompt:

                    PUSH    HL

                                                ; is there a better way of setting C to a
                                                ; non-newline? Any other regs known not
                                                ; to have this value?
                    LD      C,0
FreshStart:

                    LD      HL,NoCharClass

NLTest:
                                                ; check for newline
                    LD      A,C
                    CP_JP_Z CR,NLTestTrue

NextCharLoop:
                                                ; This code is compatable with Z80 dongle
                                                ; and Z80 emulator
                    IN      A,(IOSTAT)          ; 6850 status register
                    AND     RDRF                ; 6850 receive data register full
                    JR      Z,NextCharLoop      ; Z -> no, try again
                    IN      A,(IODATA)          ; get char
                    LD      C,A
                    OUT     (IODATA),A          ; echo

                                                ; Do we have the same class as before?
                    PUSH    HL
                    LD      HL,ClassLookup-1
                                                ; Test for quote first
                                                ; This doesn't save spave, but takes 3 bytes
                                                ; away from class lookup and puts them here
                                                ; so can be used to change odd/even of
                                                ; ...Class subroutines
                                                ; RST CompareJump
                                                ; DB 34,(LC_QuoteTestTrue-1)&0ffh
LookupClassLoop:
                    INC     L
                    CP      (HL)
                    INC     L
                    JR      C,LookupClassLoop
LC_QuoteTestTrue:
                    LD      B,(HL)
                    POP     HL

                                                ; are L and B equal?
                    LD      A,L
                    XOR     B
                                                ; Z if they are equal, NZ if not

                    JP      (HL)                ; Jump based on previous CharClass pointer

DigitClass:
                                                ; Keep tbis as a 3-byte instruction to ensure
                                                ; that QuoteClass LSB is different from
                                                ; all other class subroutines
                    JP      Z,DigitClassNotEnd

DigitClassEnd:
                                                ; Write token into program
                                                ; need to preserve DE, don't care about HL

                    EX      (SP),HL
                    LD      (HL),IntegerToken
                    INC     HL
                    LD      (HL),E
                    INC     HL
                    .db     36h                 ; opcode for LD (HL),n eats next byte
Write_Shared_AtSP:
                    POP     DE
Write_Shared:
                    LD      (HL),D
Write_Shared_Written:
                    INC     HL
                    EX      (SP),HL

NoCharClass:
                    LD      L,B
                    XOR     A                   ; set Z
                    LD      D,A                 ; reset state information
                    LD      E,A

                    JP      (HL)

DigitClassNotEnd:

                    PUSH    DE
                    EX      (SP),HL

                                                ; A is zero at this point

                                                ; Accumulate the value into D

                                                ; Muliply by 10

                    ADD     HL,HL
                    ADD     HL,HL
                    ADD     HL,DE
                    ADD     HL,HL

                                                ; Add in the new digit

                    ADD     HL,BC
                                                ; Because B has a value and C has the digit
                                                ; value+48, we need to subtract those things
                                                ; from HL
                    LD      DE,-((DigitClass&0ffh)*256+48)
                    ADD     HL,DE

                    EX      DE,HL

                    POP     HL

                    JR      NextCharLoop

QuoteClassExpEnd:

                                                ; A is equal to:
                                                ; char class (B) XOR QuoteCharClassExpEnd

                                                ; so long as QuoteCharClass is the only class
                                                ; with an odd address or the only one
                                                ; with an even address then A will only
                                                ; have LSB=1 if current char class
                                                ; is QuoteCharClass - i.e. end of string

                    .db     0e6h                ; opcode for AND n eats next byte
                                                ; (which is 2dh lsbits are 01)

QuoteClass:

                    DEC     L                   ; set to QuoteClassExpEnd

                                                ; first time through A is zero
                                                ; on fall A is even unless C is QuoteClass

                    AND     H                   ; H is 1

                                                ; Now Z is set if this was first Quote, or if
                                                ; we are in a string and haven't reached
                                                ; last quote

                                                ; carry is clear here
                    .db     0dah                ; opcode for JP C,nnnn eats next 2 bytes
LT0Class:
                    INC     HL                  ; next char should always count as
                                                ; different class
                    NOP

CompClass:
                    NOP
                    NOP
AlphaClass:

                    EX      (SP),HL
                    LD      (HL),C
                    INC     HL
                    EX      (SP),HL

                    DEC     DE                  ; increase char count

                                                ; if NZ then we will just
                                                ; have written a different class char:
                                                ; good, this ensures no spurious
                                                ; strcmp matches from leftover
                                                ; buffer contents

                                                ; now we need to decide whether to jump to:
                                                ; FreshStart - if its the last quote in
                                                ;        a string
                                                ; NLTest   - if part way through string or
                                                ;        token
                                                ; TokenClassEnd - if end of token

                    JR      Z,NLTest

                    LD      A,L
                    CP_JP_Z QuoteClassExpEnd,FreshStart

TokenClassEnd:

                                                ; Make H point to the start of the token
                                                ; to be looked up
                    EX      (SP),HL
                    ADD     HL,DE

                                                ; it's a var if bits 7,6,5 are 010 and
                                                ; E=-2
                                                ; These aren't the only conditions that
                                                ; could lead to the test below passing -
                                                ; e.g. if 7,6,5=001 and E=10011110. But E
                                                ; has to be large for this to happen, so
                                                ; quite unlikely in practice.

                    LD      A,(HL)
                    XOR     040h
                    LD      D,A
                    AND     0e0h
                    XOR     E

                    CP_JP_Z 0feh,Write_Shared

                    LD      DE,TokenList

LookupToken_Loop:
                    LD      A,(DE)
                    PUSH    AF
                    PUSH    HL
StrCmp:
                    INC     DE
                    LD      A,(DE)
                    XOR     (HL)
                                                ; iff match then A is either 00h or 80h
                                                ; (80h if last char)
                    INC     HL
                    JR      Z,StrCmp            ; match and not last char

                                                ; equal to 080h iff match and last char
                    XOR     080h
                                                ; equal to Z iff match and last char

                    POP     HL

                    JR      Z,Write_Shared_AtSP

                    POP     AF

LookupToken:
                    LD      A,(DE)
                    RLA
                    INC     E; Z set if end of TokenList
                    JR      NC,LookupToken
                    JR      NZ,LookupToken_Loop

                                                ; didn't find it

                                                ; if (HL)>=64 and (HL+1)<64 then its a var
                                                ; could do the var test here
                                                ; if it can be done in few bytes

                    LD      (HL),QuestionMarkToken&0ffh
                    JR      Z,Write_Shared_Written

                    org     01b3h
AbsSubEx:
                    EX      DE,HL
AbsSub:
                    XOR     A
                    XOR     D
                    RET     P

                    RST     NegateDE

                                                ; shared code. okay for this to go here
                                                ; because in ExpEvaluateNum, test for
                                                ; left brace is before test for token
                                                ; between first and last function
LeftBraceToken:
                    RET

UsrSub:
                    EX      DE,HL
                    JP      (HL)

PeekSub:
                    EX      DE,HL               ; EB HL = addr, DE = $00A8
                    LD      E,(HL)              ; 5E get byte
                    NOP;DEC D                   ; 15->00 D is already $00
                    RET                         ; C9

RndSub:
; LCG
; don't use low byte in return value.
; Multiplier 47989 is mentioned here:
; https://groups.google.com/g/prng/c/evszGs76o1w?pli=1

                    PUSH    DE
                    LD      HL,(RNG_SEED)
                    LD      DE,47989
                    CALL    MulSub              ; A is zero after this
                    JP      RndSubImpl

; Token values >= this are all operators
Operators:

LTESub:
                                                ; Swap operands and fall through
                    EX      DE,HL
GTESub:
                    RST     CompareHLDE
                    JR      Z,BinReturn
GTSub:
                                                ; Swap operands and fall through
                    EX      DE,HL
LTSub:
                    LD      A,L
                    SUB     E
                    LD      A,H
                    SBC     A,D
                    RRA
                    XOR     H
                    XOR     D
                    RLA

                    .db     3eh                 ; LD A,n opcode to eat next byte
EqualSub:
                    RST     CompareHLDE         ; returns Z iff HL=DE
BinReturn:
                    CCF
                    .db     3eh                 ; LD A,n opcode to eat next byte

NotEqualSub:
                    RST     CompareHLDE; returns Z iff HL=DE
                    LD      DE,1
                    RET     NC
                    DEC     DE
                    RET

AddSub:
                    .db     0d2h                ; opcode for JP NC,nnnn to eat 2 bytes
SubSub:
                    NOP
NegateSub:
                    RST     NegateDE
                                                ; Add DE to HL and keep in DE
                    ADD     HL,DE
                    EX      DE,HL

                    RET

MulSub:
; 20 bytes
; multiply HL and DE into DE, preserving B
                    PUSH    BC
                    LD      B,H
                    LD      C,L

; TODO can we switch the role of HL and DE
; to avoid the extra XCHG before RET
Multiply:
; multiply BC and DE into DE
                    LD      A,16
MulLoop:
                    ADD     HL,HL
                    EX      DE,HL
                    ADD     HL,HL
                    EX      DE,HL
                    JR      NC,DontAdd
                    ADD     HL,BC
DontAdd:

                    DEC     A
                    JR      NZ,MulLoop

                    EX      DE,HL
                    POP     BC
                    RET

; Version of DivSub based on 16-bit division
; from here:
; https://www.cpcwiki.eu/index.php/Programming:Integer_Division

DivSub:
; Divide HL by DE
; Remainder in HL
; Result in DE
                    PUSH    BC

                    LD      A,E
                    OR      D
                    CALL    Z,Error

                                                ; Are signs the same?
                    LD      A,D
                    XOR     H
                    PUSH    AF

                                                ; Get absolute value of inputs
                    CALL    AbsSubEx
                    CALL    AbsSubEx

                    LD      A,H
                    LD      C,L
                    LD      HL,0

                    LD      B,16

clcd161:
                    RL      C
                    RLA
                    ADC     HL,HL
                    SBC     HL,DE

                    JR      NC,clcd162
                    ADD     HL,DE
clcd162:
                    CCF
                    DJNZ    clcd161

                    RL      C
                    RLA
                    LD      D,A
                    LD      E,C

                                                ; At this point DE has the result and HL
                                                ; has the remainder

                    POP     AF
                    POP     BC

                                                ; If signs of inputs were the same, return now
                                                ; Note that remainder in HL is always positive
                    RET     P

                    RST     NegateDE

                    RET

LineNumSub:
                    INC     BC
                    INC     BC
                    RET

PrintSub:
PrintSubLoop:
                                                ; on call HL is address of PrintSub
                                                ; so H=1
                                                ; on subsequent passes H = 0 or QuoteChar

                    LD      A,(BC)

                    SUB     StringToken
                    JR      Z,PrintSubString

                                                ; This assumes that LinenumSub is the
                                                ; next token after StringToken
                    CP      (LastStatement-StringToken+1)&0ffh

                    DEC     H                   ; doesn't affect carry
                                                ; Z set if we need a newline

                    JR      C,PrintSubEnd

PrintSubExpression:
                    RST     ExpEvaluate
                    CALL    PrintInteger

                    SCF
PrintSubString:
                    CALL    NC,OutputString     ; carry is clear on return

                    LD      H,A                 ; A is 0 or QuoteChar

                    CP_BC   CommaToken
                    JR      Z,PrintSubLoop

                    DEC     BC

                    XOR     A

PrintSubEnd:
                    RET     NZ                  ; don't print newline if we've just had
                                                ; comma
CRLF:
                    LD      A,CR
                    RST     PutChar
                    LD      A,LF
                    RST     PutChar
                    RET

GosubSub:
                    RST     ExpEvaluate
                    POP     HL

                    PUSH    BC
                    PUSH    HL

                    .db     03eh                ; opcode for LD A,n to eat next byte
GotoSub:
                    RST     ExpEvaluate
                    CALL    GetLineNum
                    RET     Z
                    CALL    Error


InputSub:

                    CALL    GetVarLocationBVar
                    PUSH    HL

                    LD      HL,INPUT_BUFFER
                    PUSH    BC

                    PUSH    HL

                    CALL    GetLineNoPrompt

                    POP     BC
                    RST     ExpEvaluate
                    POP     BC

                                                ; fall through
POPHAssignToVar:

                    POP     HL

                                                ; Put DE into var (HL)

                    LD      (HL),E
                    INC     HL
                    LD      (HL),D

                    RET

ForSub:
                    LD      HL,ForSubImpl
                    PUSH    HL

                                                ; fall through

LetSub:
                    CALL    GetVarLocationBVar
                    PUSH    HL

                                                ; Test that we have an equals sign
                    CP_BC   EqualSub
                    CALL    NZ, Error

                    RST     ExpEvaluate

                    JR      POPHAssignToVar


IfSub:
                    RST     ExpEvaluate
                    LD      A,D
                    OR      E
                    RET     NZ

                                                ; If DE zero then fall through to next line
                    JP      AdvanceToNextLineNum

EndSub:
                    JP      Ready
                                                ; Hi byte of AdvanceToNextLineNum is 3
                                                ; which is opcode for INR B : harmless
EndProgram          .equ    EndSub-1

ExecuteProgram:
                                                ; Point BC to first line
                    LD      BC,PROG_BASE

ExecuteProgramLoop:
                    LD      A,(BC)

ExecuteDirect:

                    SUB     LineNumSub&0ffh

                                                ; Check that it is a token between
                                                ; LinenumSub and LastStatement
                    CP      (LastStatement-LineNumSub+1)&0ffh
                    CALL    NC,Error

                    INC     BC

                    ADD     A,LineNumSub&0ffh

                                                ; Carry is clear now
                                                ; Sign is set/reset according to the
                                                ; address of the statement Sub
                                                ; e.g. for LineNumSub Sign is clear

                                                ; Put return address onto stack
                    LD      HL,ExecuteProgramLoop
                    PUSH    HL

                                                ; Put pointer to call address into HL
                    LD      L,A
                                                ; ExecuteProgramLoop must be on the same page
                                                ; page as PrintSub so that we don't have to
                                                ; update H

                                                ; Jump to it
                                                ; Carry is clear when we do this
                    JP      (HL)

NewSub:
                    RST     0

ListSub:
                    JP      ListSubImpl

ReturnSub:
                                                ; Expect stack size to be 6 or more
                                                ; any less and we have return without gosub
                    LD      HL,-(STACK_INIT-6)-1
                    ADD     HL,SP
                    CALL    C,Error

                    POP     HL                  ; Get return address first
                    POP     BC                  ; Get pointer to program loc to ret to
                    JP      (HL); instead of RET

LastStatement:
NextSub:
                    POP     HL                  ; discard return address
                                                ; stack contains <SP> VL+1,S,-T,LS,EPL
                    POP     HL                  ; get VL+1
                                                ; stack contains VL+1 <SP> S,-T,LS,EPL
                    LD      D,(HL)
                    DEC     HL
                    LD      E,(HL)

                    EX      (SP),HL             ; step is in HL, VL is in (SP)
                                                ; stack contains VL+1 <SP> VL, -T,LS,EPL
                    EX      DE,HL               ; step is in DE, var value in HL
                    ADD     HL,DE               ; add step onto var
                    EX      DE,HL               ; result is in DE, step is in HL
                    EX      (SP),HL             ; step is in (SP), VL is in HL
                                                ; stack contains VL+1 <SP> S, -T,LS,EPL

                    LD      (HL),E              ; put back into VL
                    INC     HL                  ; H = VL+1
                    LD      (HL),D              ; DE now has loop var value (LV)

                    POP     AF                  ; get step so that hi bit of A has
                                                ; sign of step
                    POP     HL                  ; get -T
                                                ; stack contains VL+1,S,-T <SP> LS,EPL

                    ADD     HL,DE               ; HL now has LV-T

                    XOR     H                   ; xor sign of step with
                                                ; sign of result

                                                ; if result of xor above is 1
                                                ; then keep looping, or if HL
                                                ; is zero then keep looping

                    POP     DE                  ; this is LoopStart
                                                ; stack contains VL+1,S,-T,LS <SP> EPL

                    JP      M,NextSubLoop

                    LD      A,H
                    OR      L
                    RET     NZ

NextSubLoop:

                    LD      B,D
                    LD      C,E
                    LD      HL,-10
                    ADD     HL,SP
                    LD      SP,HL

                    RET

; ( ) , TO STEP tokens must have values between
; statements and functions

ToToken             .equ    LastStatement+1
StepToken           .equ    LastStatement+2
RightBraceToken     .equ    LastStatement+3
CommaToken          .equ    LastStatement+4

ClassLookup:
                    .db     64,AlphaClass&0ffh
                    .db     58,CompClass&0ffh
                    .db     48,DigitClass&0ffh
                    .db     35,LT0Class&0ffh
                    .db     34,QuoteClass&0ffh
                    .db     33,LT0Class&0ffh
                    .db     0,FreshStart&0ffh

RndSubImpl:
                    EX      DE,HL
                    INC     HL
                    LD      (RNG_SEED),HL
                                                ; Use only the high byte to get a value
                                                ; between 0 and 255
                    LD      L,H
                    LD      H,A                 ;
                    POP     DE

                                                ; TODO if implementing modulus operator, this can be the enty point
                    CALL    DivSub
                    EX      DE,HL
                    RET

; This 8 byte routine can be moved anywhere to
; fill holes. It needs a RET on the same
; page to jump to
OutputString:
; Pointer in B points to string token marker
                    INC     BC
OutputStringLoop:
                    CP_BC   StringToken
                    JR      Z,OutputStringRet
OutputString_WithQuote:
                    RST     PutChar
                    JR      Z,OutputStringLoop

; This 20 byte routine can be moved as needed
GetVarLocationBVar:
                    CP_BC   32                  ; Test that we have a var
                    CALL    NC,Error

GetVarLocation:
; A should contain a var token
; and B points to tbe location after
; the var token
; return with var address in HL
; and B pointing to next char
; A will never be 255 on return

                    LD      H,VAR_SPACE/256
                    ADD     A,A
                    LD      L,A

                    RET     NZ

                                                ; fall through if it is array var

                    CALL    ExpBracketedB

                                                ; Now DE contains the array index
                                                ; Add it twice to get the offset

                    LD      HL,(PROG_PARSE_PTR)
                    INC     HL                  ; up 1 byte to avoid EndProgram marker
                    ADD     HL,DE
                    ADD     HL,DE
OutputStringRet:
                    RET

GetLineNum:
                                                ; Line number is in DE, look it up in the
                                                ; program and set BC to point to the LinenumSub
                                                ; token.
                                                ;
                                                ; DE is preserved
                                                ; H is preserved
                                                ; L is not preserved
                                                ;
                                                ; return with Z set if successful
                                                ;
                                                ; Z clear if not successful, and BC points
                                                ; to the first byte of the line with number
                                                ; greater than the request
                                                ;
                                                ; Carry is always clear on return

                    LD      BC,PROG_BASE-1      ; 1 bytes before PROG_BASE

GetLineNumLoop:
                    CALL    ATNLN_INXB          ; has one INX B preceeding
                    RET     NZ

                    INC     BC

                                                ; Test for DE <= (BC), and return if true
                    LD      A,(BC)
                    INC     BC
                    SUB     E
                    LD      L,A
                    LD      A,(BC)
                    SBC     A,D                 ; C set if DE > (BC), and Z not set
                                                ; C clear if DE <= (BC)
                    JR      C,GetLineNumLoop

                    DEC     BC
                    DEC     BC
                                                ; Now we want Z set if DE=(BC), clear
                                                ; otherwise

ATNLN_RetNZ:                                    ; shared code. Returns NZ if we know
                                                ; that A is non-zero

                    OR      L                   ; Carry will be cleared

                    RET

ATNLN_String:
                    CP_BC   StringToken
                    JR      NZ, ATNLN_String

                    .db     0c2h                ; opcode for JP NZ,nnnn eats 2 bytes
ATNLN_Int:                                      ; Z is always set when we reach here
                    INC     BC
ATNLN_INXB:
                    INC     BC

AdvanceToNextLineNum:
; BC is a pointer to somewhere in the program.
; Move onto the next line number.
; Return with Z set if successful,
; Z clear if fell off end of program

                    LD      A,(BC)
                    CP_JP_Z EndProgram,ATNLN_RetNZ
                                                ; fell off end of program

                    CP      LinenumSub&0ffh
                    RET     Z; carry will be clear if we return here

                    INC     BC

                    CP_JP_Z IntegerToken,ATNLN_Int
                    CP_JP_Z StringToken,ATNLN_String
                    JR      AdvanceToNextLineNum

ListSubImpl:
                    LD      BC,PROG_BASE
ListLoop:
                    LD      A,' '
                    RST     PutChar

                    CP_BC   EndProgram
                    RET     Z

                    LD      HL,ListLoop         ; so that we can loop using RET
                    PUSH    HL

                                                ; H is already set to the correct page
                    LD      L,TokenList&0ffh-1

                                                ; These need to be on same page
                    CP_JP_Z StringToken,List_String
                    CP_JP_Z LinenumSub,List_Linenum
                    CP_JP_Z IntegerToken,List_Integer

                    JR      C,List_Var

                                                ; No need to check for end of TokenList
                                                ; impossible not to be a token value in A

List_Token_Loop:
                    LD      D,(HL)
                    INC     D
                    INC     HL
                    JP      P,List_Token_Loop

List_Token:
                                                ; on entry, A contains the token
                                                ; so must not use A during this loop
                    CP      (HL)
                    INC     HL
                    JR      NZ,List_Token_Loop

List_Token_String_Loop:
                    LD      A,(HL)
                    AND     07fh
                    RST     PutChar
                    OR      (HL)
                    INC     HL
                    RET     M
                    JR      List_Token_String_Loop

List_LineNum:
                    CALL    CRLF

List_Integer:
                    LD      A,(BC)
                    INC     BC
                    LD      E,A
                    LD      A,(BC)
                    INC     BC
                    LD      D,A
                                                ; fall through to PrintInteger

; Output the value in DE
PrintInteger:
                    XOR     A
                    PUSH    AF                  ; end marker is Z flag

                    OR      D                   ; S is set if -ve

                    JP      P,PrintIntegerLoop
                    LD      A,'-'
                    RST     Putchar

PrintIntegerLoop:
                                                ; doesn't matter whether HL is -ve or +ve
                                                ; because remainder from DivSub behaves as
                                                ; though it is +ve

                    EX      DE,HL
                    LD      DE,10

                    CALL    DivSub
                                                ; HL contains remainder after / 10
                                                ; DE contains the quotient

                    LD      A,'0'
                    ADD     A,L
                    PUSH    AF                  ; push onto stack

                                                ; if DE is zero we are done
                    LD      A,D
                    OR      E
                    JR      NZ,PrintIntegerLoop

PrintIntegerLoop2:
                    POP     AF
                    RET     Z                   ; Z is set on return, HL<=0
                    RST     PutChar
                    JR      Z,PrintIntegerLoop2

List_String:
                    CALL    OutputString_WithQuote

                    .db     011h                ; LD DE,nnnn skips 2 bytes
List_Var:
                    ADD     A,'@'
                    RST     PutChar
                    RET

; byte before TokenList must have high bit set
; e.g. RET

                    org     0388h
                    .db     128

; order in this list must make sure that a
; token A that is a left substring of another
; token B appears later in the list than B
; e.g. < is after <=

                    org     0389h

TokenList:
                    .db     QuestionMarkToken&0ffh
                    .db     '?'+128
                    .db     PrintSub&0ffh
                    .db     "PRIN",'T'+128
                    .db     LetSub&0ffh
                    .db     "LE",'T'+128
                    .db     GotoSub&0ffh
                    .db     "GOT",'O'+128
                    .db     GosubSub&0ffh
                    .db     "GOSU",'B'+128
                    .db     ReturnSub&0ffh
                    .db     "RETUR",'N'+128
                    .db     InputSub&0ffh
                    .db     "INPU",'T'+128
                    .db     ForSub&0ffh
                    .db     "FO",'R'+128
                    .db     NextSub&0ffh
                    .db     "NEX",'T'+128
                    .db     IfSub&0ffh
                    .db     "I",'F'+128
                    .db     EndSub&0ffh
                    .db     "EN",'D'+128

; Before this are keywords allowed at run-time
                    .db     ExecuteProgram&0ffh
                    .db     "RU",'N'+128
                    .db     ListSub&0ffh
                    .db     "LIS",'T'+128
                    .db     NewSub&0ffh
                    .db     "NE",'W'+128

; before operators are non-statement
; non-operator tokens

                    .db     AbsSub&0ffh
                    .db     "AB",'S'+128
                    .db     UsrSub&0ffh
                    .db     "US",'R'+128
                    .db     PeekSub&0ffh
                    .db     "PEE",'K'+128
                    .db     RndSub&0ffh
                    .db     "RN",'D'+128

                    .db     ToToken&0ffh
                    .db     "T",'O'+128
                    .db     StepToken&0ffh
                    .db     "STE",'P'+128
                    .db     CommaToken
                    .db     ','+128
                    .db     LeftBraceToken&0ffh
                    .db     '('+128
                    .db     RightBraceToken&0ffh
                    .db     ')'+128
                    .db     EqualSub&0ffh
                    .db     '='+128
                    .db     NotEqualSub&0ffh
                    .db     "<",'>'+128
                    .db     GTESub&0ffh
                    .db     ">",'='+128
                    .db     LTESub&0ffh
                    .db     "<",'='+128
                    .db     LTSub&0ffh
                    .db     '<'+128
                    .db     GTSub&0ffh
                    .db     '>'+128
                    .db     AddSub&0ffh
                    .db     '+'+128
                    .db     SubSub&0ffh
                    .db     '-'+128
                    .db     MulSub&0ffh
                    .db     '*'+128
                    .db     DivSub&0ffh
                    .db     '/'+128
