; Will Stevens
; 25th Feb 2023
; 1K 8080 BASIC
;
; 2023-03-03 About 450 bytes long
; 2023-03-08 About 750 bytes long
; 2023-03-11 About 840 bytes long
; 2023-03-12 About 930 bytes long.
;   String tokens added.
;   * and / yet to be added.
;   Some scope for size optimization.
; 2023-03-12 About 940 bytes long
;		* added
;		some reduction in code size done
; 2023-03-16 About 970 bytes long
;   unsigned / and integer output added
;		about 30 bytes could be saved
;		by using RST in place of call
;   in some places
; 2023-03-17 About 940 bytes long
; 2023-03-17 About 970 bytes long
;		INPUT added 
; 2023-03-19 About 960 bytes long
;    some bug fixes
;		 capable of playing lunar lander
; 2023-03-22 About 970 bytes long
;		 signed / added


		
; Memory map:
; system vars
; var space : 52 bytes
; input buffer : 64 bytes
; (also used as expression stack)
; program area
; stack area - top of RAM

; For development purposes assume we have
; 1K ROM from 0000h-03FFh containing BASIC
; 1K RAM from 0400h-07FFh

RAM_TOP equ 0800h ; 1 more than top byte of RAM

; Token values
; 0-25 are variables
; StringToken ; 26
IntegerToken equ 28 ; followed by 16-bit integer
LinenumToken equ 27 ; followed by 16-bit integer
StringToken equ 26 ; followed by 1 byte length, followed by string characters
CommaToken equ 29
NotFoundToken equ 30

; Callable tokens are low byte of subroutine to call

; Errors are display as Ex where x is a letter
; Error code is the lowest 16 bits of the
; address that called RST 3. If there is a
; collision the shuffling subroutine might
; resolve it.

; Input buffer and operator stack can share the
; same memory - not used at same time
; Input buffer must not be over a 256 byte 
; boundary
; Stack is just below input buffer to save code
; when initialising

org RAM_TOP-64
STACK_INIT:
INPUT_BUFFER:
OPERATOR_STACK_PTR:
	DW 0
OPERATOR_STACK_BASE:
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

INPUT_BUFFER_END:

ORG 0400h

; this must be on a 256 byte boundary
VAR_SPACE:
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0
	
PROG_PTR:
	DW PROG_BASE
PROG_PARSE_PTR:
	DW PROG_BASE

PROG_BASE:

ORG 00h
	RST 3	; call error routine
				; which will fall through to Ready
	
; Space for 7 byte subroutine


ORG 08h

; PutChar is called frequently
; it can be called using RST 1

PutChar:
	; Assume that port 0 is for char I/O
	; And port 1 is for char ready status
	
	OUT 0
	RET

; Space for 5 byte subroutine(s)
PrintDigit:
	MOV A,E
	ADI '0'
	
	RST 1
	RET


ORG 10h

; GetChar can be called with RST 2

GetChar:
	IN 1
	JZ GetChar
	IN 0
	RET

ORG 18h

;Display error code and go back to line entry
Error:
	MVI A,10
	RST 1
	MVI A,69
	RST 1
	POP PSW		; discard return address and
						; get error code
	ANI 0fh
	ADI 'A'
	RST 1
	MVI A,10
	RST 1
	
Ready:
	; Set stack pointer to just below input buffer
	; Do this every time to guard against
	; GOSUB with no RETURN errors
	
	LXI H,INPUT_BUFFER-1
	SPHL
	
	; H is also the initial pointer to input buffer

	CALL Getline
	
	; Now we have a line terminated by chr(10)
	; Get location of input buffer into HL
	LXI H,INPUT_BUFFER

	CALL NextToken
	
	; If first token was an int, change it to a line no. marker and add the line to the program
	
	LHLD PROG_PTR
	MOV A,M
	CPI IntegerToken
	JNZ ExecuteDirect
	DCR A ; change int to linenum
	MOV M,A
	LHLD PROG_PARSE_PTR
	SHLD PROG_PTR
	
	JMP Ready

GetLine:
	INX H
	MOV A,L
	CPI INPUT_BUFFER_END&0ffh
	CZ Error	; input buffer overflow
	RST 2
	
	MOV M,A
	
	CPI 10
	JNZ GetLine
	RET

ExecuteDirect: ; Depth = 0
	
	LHLD PROG_PTR
	SHLD PROG_PARSE_PTR
	
	; Otherwise execute the statement
	; Assume its RUN
	JMP ExecuteProgram
	
	JMP Ready
	
NextToken: ; Depth = 1
	; Get class of first char
	MOV A,M
	CPI 10
	RZ
	CALL CharClass
	MOV B,A
	
	; Copy HL to DE so that DE points to start
	PUSH H
	POP D
	
	INX H
	
NextChar:
	; Are we in a string
	MOV A,B
	CPI '"'
	JNZ NotInString
	
	; If yes then is M a chr(10) or quote
	MOV A,M
	INX H
	CPI 10
	JZ Error
	CPI '"'
	JNZ NextChar
	
	; Otherwise fall through and whole string will be handled
	
NotInString:
	; Is M a different class from B, or
	; a char that can't belong to a sequence
	MOV A,M
	CALL CharClass
	
	CMP B
	JNZ DiffClass
	ANI 80h
	JZ DiffClass
	
	INX H
	JMP NextChar

; If we reach this point then contents of buffer from DE to HL-1 are a token. C is the length
; If it is an integer then calculate value
; Otherwise lookup the token
DiffClass:
	; Set the hi bit of the last char in the token
	DCX H
	MOV A,M
	ORI 128
	MOV M,A
	INX H
	
	MOV A,B
	CPI ' '
	JZ NextToken
	CPI '0'+128
	JZ Integer
	CPI '"'
	JZ String
	CPI 'P'+128
	JNZ NotVar
	LDAX D
	SUI 'A'+128	; if hi bit is set then length=1
	JNC Var			; and it must be a var
	
NotVar:
	
	CALL LookupToken
	CPI NotFoundToken
	CZ Error

	PUSH H
	
; Store token in program
	LHLD PROG_PARSE_PTR
	MOV M,A
	INX H
	SHLD PROG_PARSE_PTR
	
	POP H
	
	JMP NextToken

Var:
	; Token value is var letter-'A'
	
	; Store var token in program
	LHLD PROG_PARSE_PTR
	MOV M,A
	INX H
	SHLD PROG_PARSE_PTR
	
	; DE points to the single-char varname, so swap it with HL and then inc to get a pointer to the next char to look at
	XCHG
	INX H
	
	JMP NextToken

ParseInteger:
	; DE points to integer
	; The integer will be constructed in HL
	; on return DE points to char after int

	LXI H,0
	
ParseIntegerNext:
	; Muliply by 10
	PUSH H
	POP B
	
	DAD H
	DAD H
	DAD B
	DAD H

	LDAX D
	INX D
	SUI '0'
	
	MVI B,0
	MOV C,A
	DAD B
	
	JP ParseIntegerNext
	
	; Subtract 128 because fhe last byte had hi bit set
	LXI B,0ff80h
	DAD B
	
	RET
	
Integer:
	CALL ParseInteger
	
	; At this point HL contains the integer
	; DE points to tbe char after end of token
	
	PUSH D
	XCHG
	
	; Store integer in program
	LHLD PROG_PARSE_PTR
	MVI M,IntegerToken
	INX H
	MOV M,E	; TODO code in common with var assign
	INX H
	MOV M,D
	INX H
	SHLD PROG_PARSE_PTR
	
	POP H
	
	JMP NextToken

; DE points to first double quote
; HL-1 is last double quote
String:
	INR E
	MOV A,L
	SUB E
	DCR A
	;A contains the length
	
	PUSH H
	
	;Store token ID, length and string
	LHLD PROG_PARSE_PTR
	MVI M,StringToken
	INX H
	MOV M,A
	INX H
	CALL StrCpy
	SHLD PROG_PARSE_PTR
	
	POP H
	
	JMP NextToken
	

; DE points to start of token
; HL points 1 char after
LookupToken: ; Depth = 2
	PUSH H
	
	LXI H,TokenList-1
LookupTokenNext:
	INX H
	MOV A,L
	CPI NotFoundAddr&0ffh
	JZ NotFoundToken
	PUSH D
	CALL Strcmp
	POP D
	CPI 080h
	JZ FoundToken
	
LookupTokenFindNext:
	MOV A,M
	INX H
	ANI 128
	JNZ LookupTokenNext
	JMP LookupTokenFindNext

NotFoundToken:
FoundToken:
; HL points to the last char of the token we've found
; Advance past this then get the token value
	INX H
	MOV A,M
	
	POP H
	RET

; DE points to hi-bit terminated string
; HL points to hi-bit terminated string
; Returns 128 in A on match
; Can't have 128 as a character in D
; (so assume that strings must be ASCII)
Strcmp: ; Depth = 3
	LDAX D
	CMP M
	RNZ
	
	ANI 128
	RNZ
	
	INX D
	INX H
	JMP Strcmp

; DE points to start
; HL points to dest
; A is length (max 127 bytes)

; Leaves HL pointing to char after string
StrCpy:
	DCR A
	RM		; DCR doesn't affect carry, but does 
				; affect sign bit, so 
	
	PUSH PSW
	LDAX D
	MOV M,A
	POP PSW

	INX D
	INX H
	
	JMP Strcpy
	


; Return the class of a character for tokenizing
; Digit
; Alphabetical
; All others are distinct classes

CharClass:
	CPI '0'
	RC	 			; LT '0' then return
	CPI 'Z'+1
	RNC				; GT Z then return
	CPI '9'+1
	JC Digit	; LTE 9 and it is a digit	
	CPI '<'
	RC				; LT '<' then return
	CPI '>'+1	; LTE '>' and its comp operator
	JC CompOp
	CPI 'A'
	RC 				; LT 'A' then return
	

	; Otherwise fall through
	
Alpha:
Digit:
; on entry A will be 0-9 or A-Z
; on exit it will be either 0 or P with high bit set
	ANI 60h
	ADI 90h
	RET
	
CompOp:
; on entry A will be < (60), = (61), > (62)
; on exit it will be < with high bit set
	ANI 3ch
	ORI 80h
	RET

; TODO base it on looking for 
; hi bit at end of string and inline
; outputstring
; this could save about 10 bytes
PrintStringToken:
	INX B
	LDAX B
	INX B
	
	CALL OutputString
	JMP PrintSubEndTest
	
OutputString:
;Pointer in B
;Length in A

	ORA A
	RZ
	
	PUSH PSW
	LDAX B
	RST 1
	POP PSW
	
	INX B
	
	DCR A
	JMP OutputString
	
GetLineNum:
	; Line number is in DE, look it up in the program and set BC to the position after it
	LXI B,PROG_BASE

GetLineNumLoop:
	INX B
	
	; Test for (BC)=DE, and return if true
	; (after advancing BC to next token)
	LDAX B
	SUB E
	MOV L,A
	INX B
	LDAX B
	INX B		; advance
	SUB D
	ORA L
	RZ
	
GetLineNumNext:
	CALL AdvanceToNextLineNum
	JMP GetLineNumLoop
	
AdvanceToNextLineNum:
; BC is a pointer to somewhere in the program
; move onto the next line number
; or error if we fall out of program
	LDAX B
	
	SBI LinenumToken
	RZ
	
	INR A
	CPI (IntegerToken-LinenumToken+1)
	JZ ATNLN_Int
	CPI (StringToken-LinenumToken+1)
	JNZ ATNLN_NotInt 
	
ATNLN_String:
	INX B
	LDAX B
ATNLN_Int:
	; Add A onto BC
	ADD C
	MOV C,A
	
	MVI A,0
	ADC B
	MOV B,A
	
ATNLN_NotInt:
	INX B
	
	LHLD PROG_PTR
	MOV A,L
	SUB C
	MOV L,A
	MOV A,H
	SUB B
	ORA L
	JNZ AdvanceToNextLineNum
	
	; Error, fell off end of program
	RST 3

ExecuteProgram: ; Depth = 0
	; Point BC to first line
	; Skip over the line number
	LXI B,PROG_BASE+3

ExecuteProgramLoop:
	; Check that we haven't reached end of program
	LHLD PROG_PTR
	MOV A,L
	CMP C
	JNZ ExecuteProgramNotEnd
	MOV A,H
	CMP B
	JZ Ready
	
	; TODO need to check that we haven't ended with a GOSUB without return condition
	; stack should be empty
	; if not then correct it
	
ExecuteProgramNotEnd:
	LDAX B
	
	; Is it a line number?
	CPI LinenumToken
	JNZ ExecuteProgramNotLineNum
	
	INX B
	INX B
	INX B
	LDAX B
	
ExecuteProgramNotLineNum:
	INX B

	; TODO Check that it is a keyword allowed in a program
	
	; Put return address onto stack
	LXI H,ExecuteProgramLoop
	PUSH H
	
	; Put pointer to call address into HL
	MOV L,A
	MVI H,PrintSub/256
	
	; Jump to it
	PCHL

;Output the value in DE
PrintInteger:
	MOV A,D
	ORA A
	JP PrintIntegerSkipNeg
	MVI A,'-'
	RST 1
	CALL NegateDE
	; TODO above doesnt work for the calue -32768
	; need to wait until we have signed division to fix
	
PrintIntegerSkipNeg:
	MVI A,128 ; Hi bit set means no non-zero yet
	XCHG
	LXI D,10000
	CALL DivideHLPrint
	LXI D,1000
	CALL DivideHLPrint
	LXI D,100
	CALL DivideHLPrint
	LXI D,10
	CALL DivideHLPrint
	MVI E,1
	XRA A	; last digit, so print it regardless of
			  ; whether we have already had non-zero
			  ; digits
	
DivideHLPrint:
	PUSH PSW
	CALL DivideHL
	POP PSW
	XRI 128
	CMP E
	JNZ PrintDigit
	
	XRI 128
	RET

; BC points to program
; DE contains value
; operator stack pointer stored in mem
; in OPERATOR_STACK_PTR
; Operand stack - SP

ExpEvaluate:
	LXI H,OPERATOR_STACK_BASE
	SHLD OPERATOR_STACK_PTR

ExpEvaluateNum:
	; Expecting ( var integer
	LDAX B
	CPI LeftBraceSub&0ffh
	JZ ExpLeftBrace
	CPI IntegerToken
	JZ ExpInteger
	CPI 26
	CNC Error
	
	; Fall through to ExpVar
ExpVar:
	; Get var value into DE
	
	MVI H,VAR_SPACE/256
	ADD A
	MOV L,A
	
	MOV E,M
	INX H
	MOV D,M
	
	INX B
	JMP ExpEvaluateOp
	
ExpLeftBrace:
	; push it onto operator stack
	LHLD OPERATOR_STACK_PTR
	MOV M,A
	INX H
	SHLD OPERATOR_STACK_PTR
	
	INX B
	JMP ExpEvaluateNum

ExpInteger:
	INX B
	LDAX B
	MOV E,A
	INX B
	LDAX B
	MOV D,A
	INX B
	
	; Fall through to ExpEvaluateOp
	
ExpEvaluateOp:
	;Expecting operator or right bracket or
	;end of expression
	
	;Are there operators on the stack?
	LDA OPERATOR_STACK_PTR
	CPI OPERATOR_STACK_BASE&0ffh
	JZ SkipExpApplyOp
	
	LDAX B
	
	CPI (RightBraceToken&0ffh)+1 ; operators or right bracket
	; Is it the end of the expression or a right bracket
	JC ExpApplyOp
	
	; or does operator on stack have GTE precedence?
	DCR A
	LHLD OPERATOR_STACK_PTR
	DCX H
	CMP M
	
	JNC SkipExpApplyOp ; no, dont apply op

ExpApplyOp:
	; POP operator from stack
	; this also handles the case when a
	; left brace is encountered
	
	LHLD OPERATOR_STACK_PTR
	DCX H
	MOV A,M
	SHLD OPERATOR_STACK_PTR
	
	MVI H,PrintSub/256
	MOV L,A
	
	; Push the operator function address
	; onto the stack, and use RET to branch to
	; it in a moment
	PUSH H
	; Put the address that we want to return to
	; into HL. 
	LXI H,ExpEvaluateOp
	
	RET	; Jump to operator function
	
SkipExpApplyOp:
	LDAX B
	
	CPI RightBraceToken&0ffh ; operators or right bracket
	; Is it the end of the expression?
	RC
	
	; Push onto the operator stack
	LHLD OPERATOR_STACK_PTR
	MOV M,A
	INX H
	SHLD OPERATOR_STACK_PTR

	; move onto next token
	INX B
	
	;because we are
	;expecting another value, push DE onto stack
	PUSH D
	
	JMP ExpEvaluateNum

NotEqualSub_1:
	; SubSub will have been executed prior to this
	; so test for zero
	LXI H,0ffffh
	DAD D
	CC NotEqualSub_2
	JMP ExpEvaluateOp
	
NotEqualSub_2:
	LXI D,1
	RET
	
EqualSub_1:
	; SubSub will have been executed prior to this
	; so test for zero
	LXI H,0ffffh
	DAD D
	INX D
	CC EqualSub_2
	JMP ExpEvaluateOp

EqualSub_2:
	LXI D,0
	RET
	
GTESub_1:
	; SubSub will have been executed prior to this
	; If hi bit of D is clear return 1, else 0
	LXI H,08000h
	DAD D
	LXI D,1
	CC EqualSub_2
	JMP ExpEvaluateOp

NegateDE:
	MOV A,E
	CMA
	MOV E,A
	MOV A,D
	CMA
	MOV D,A
	INX D
	
	RET
	
; TokenList must be on same page and index to subroutine address must not overlap with other token values
ORG 02d0h

TokenList:
	DB "PRIN",'T'+128
	DB PrintSub&0ffh
	DB "LE",'T'+128
	DB LetSub&0ffh
	DB "GOT",'O'+128
	DB GotoSub&0ffh
	DB "GOSU",'B'+128
	DB GosubSub&0ffh
	DB "RETUR",'N'+128
	DB ReturnSub&0ffh
	DB "I",'F'+128
	DB IfSub&0ffh
	DB "INPU",'T'+128
	DB InputSub&0ffh
	DB "EN",'D'+128
	DB 0
; Before this are keywords allowed at run-time
	DB "RU",'N'+128
	DB 0
	DB "LIS",'T'+128
	DB 0
	DB "NE",'W'+128
	DB  0
	DB ','+128
	DB CommaToken
	DB '('+128
	DB LeftBraceSub&0ffh
;After this label, only expect things which count as operators
	DB ')'+128
	DB RightBraceToken&0ffh
	DB '<'+128
	DB LTSub&0ffh
	DB '>'+128
	DB GTSub&0ffh
	DB ">",'='+128
	DB GTESub&0ffh
	DB "<",'='+128
	DB LTESub&0ffh
	DB '='+128
	DB EqualSub&0ffh
	DB "<>"+128
	DB NotEqualSub&0ffh
	DB '+'+128
	DB AddSub&0ffh
	DB '-'+128
	DB SubSub&0ffh
	DB '*'+128
	DB MulSub&0ffh
	DB '/'+128
	DB DivSub&0ffh
NotFoundAddr:
	DB NotFoundToken
	
PrintSub:
	LDAX B
	CPI StringToken
	JZ PrintStringToken
	CALL ExpEvaluate
	PUSH B
	CALL PrintInteger
	POP B

PrintSubEndTest:
	LDAX B
	INX B
	CPI CommaToken
	JZ PrintSub
	DCX B
	MVI A,10
	RST 1
	RET

LetSub:
	; TODO test that we have var and equal sign
	LDAX B
	PUSH PSW
	INX B
	INX B
	
	CALL ExpEvaluate
	
	POP PSW
	
AssignToVar:
	; Put DE into var
	
	MVI H,VAR_SPACE/256
	ADD A
	MOV L,A
	
	MOV M,E
	INX H
	MOV M,D
	
	RET
	
GotoSub:
	CALL ExpEvaluate
	JMP GetLineNum
	
GosubSub: ; Depth = 1
	CALL ExpEvaluate
	POP H	; Preserve return address
	PUSH B
	PUSH H
	JMP GetLineNum
	
ReturnSub:
	;TODO - how to always detect
	; return without GOSUB
	; could put marker word onto stack?
	; and inc/dec every call/return
	; and check for marker march
	
	POP H	; Get return address first
	POP B ; Get pointer to program loc to return to
	PCHL

IfSub:
	CALL ExpEvaluate
	MOV A,E
	ORA D
	RNZ

	; If DE zero then fall through to next line
	JMP AdvanceToNextLineNum

InputSub:
	; TODO there is no check for var token
	; nor for integer input
	
	LXI H,INPUT_BUFFER-1
	CALL GetLine
	DCX H
	MOV A,M
	ORI 128
	MOV M,A
	
	LXI D,INPUT_BUFFER
	
	PUSH B
	CALL ParseInteger
	XCHG
	POP B
	
	LDAX B
	INX B
	
	JMP AssignToVar
	

LeftBraceSub:
	LDAX B
	; Is current operator a right brace?
	CPI RightBraceToken&0ffh
	CNZ Error ; expecting right brace
	INX B
	
; This is a dummy label
; to make sure that right brace token value is between LeftBraceSub and LTSub
RightBraceToken:

	JMP ExpEvaluateOp

LTSub:
	; Swap operands and fall through
	XCHG
	XTHL
	XCHG
GTSub:
	INX D
	JMP GTESub

LTESub:
	; Swap operands and fall through
	XCHG
	XTHL
	XCHG
GTESub:
	LXI H,GTESub_1
	JMP SubSub

EqualSub:
	LXI H,EqualSub_1
	JMP SubSub
	
NotEqualSub:
	LXI H,NotEqualSub_1
	JMP SubSub

AddSub:
	; exchange H with stack top to get operand into H and return address on stack
	XTHL

	;Add DE to top of stack and keep in DE
	DAD D
	XCHG
	
	RET

SubSub:
	CALL NegateDE
	JMP AddSub

MulSub:
	; exchange H with stack top to get operand into H and return address on stack
	XTHL
	PUSH B
	MOV B,H
	MOV C,L

Multiply:
;multiply BC and DE into HL

	MVI A,16
	LXI H,0
MulLoop:
	DAD H
	XCHG
	DAD H
	XCHG
	JNC DontAdd
	DAD B
DontAdd:

	DCR A
	JNZ MulLoop
	
	XCHG
	POP B
	RET

DivSub:
;Divide (SP) by DE
;Remainder in HL
;Result in DE

; exchange H with stack top to get operand into H and return address on stack
	XTHL

DivideHL:
;Divide HL by DE

	; Make HL +ve
MOV A,H
ORA A
PUSH PSW
XCHG
CM NegateDE
XCHG
	
	; Make DE -ve
MOV A,D
ORA A
PUSH PSW
CP NegateDE
;CALL NegateDE
	
;Divide HL by DE
;Assuming that HL is +ve and DE is -ve

	PUSH B
	LXI B,0ffffh ; Accumulator starts at -1
	
DivLoop:
	INX B
	DAD D
	JC DivLoop

; Assume we want +ve remainder
 	CALL NegateDE
 	DAD D


	PUSH B
	POP D
	
	POP B
	
	POP PSW
	CP NegateDE
	POP PSW
	CP NegateDE
	
	RET
