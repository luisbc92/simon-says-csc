	.cdecls C, LIST, "msp430g2553.h"

;-----------------------------------------------------------------------
;	Registers
;		R12-15 are reserved as working variables in functions
;		Returns should be done at R15
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;	Variables
;-----------------------------------------------------------------------
			; TIME-KEEPING
			.bss	time, 2						; 16-bit

			; PSEUDO-RANDOM NUMBER GENERATOR
			.bss	randA, 2					; 16-bit
			.bss	randB, 2					; 16-bit
			.bss	randC, 2					; 16-bit
			.bss	randX, 2					; 16-bit

			; SEQUENCE AND CONTROL
			.bss	seq, 32						; 32 * 8-bit array
			.bss	seqPos, 2					; 16-bit
			.bss	seqTop, 2					; 16-bit

			; SKILL LEVEL
			.bss	ledOnTime, 2				; 16-bit
			.bss	ledOffTime, 2				; 16-bit
			.bss	btnTime, 2					; 16-bit

			; GAME STATE MACHINE
			.global	state
			.bss	state, 2					; 16-bit
			.bss	score, 2					; 16-bit

			; LCD DRIVER
			.bss	lcdBuffer, 32				; 32 * 8-bit array

;-----------------------------------------------------------------------
;	Strings
;-----------------------------------------------------------------------

_welcome	.string	"MSP430 Digi-GameSimon Says      ", 0
_skilllvl	.string	"Choose Skill LvlE    M    H    X", 0
_extreme	.string	"     EXTREME    ", 0
_hard		.string	"      HARD      ", 0
_medium		.string "     MEDIUM     ", 0
_easy		.string "      EASY      ", 0
_score		.string "Your Score                      ", 0
_end		.string "ROUNDS ->       SCORE ->        ", 0

;-----------------------------------------------------------------------
;	Skill Level Timings and String
;-----------------------------------------------------------------------

_skString	.word	_easy, _medium, _hard, _extreme
_skOnTime	.word	200, 150, 100, 60
_skOffTime	.word	400, 300, 200, 120
_skBtnTime	.word	1500, 800, 600, 400

;-----------------------------------------------------------------------
;	Definitions
;-----------------------------------------------------------------------
RESET		.equ	0
SKILL_LEVEL	.equ	1
GEN_SEQ		.equ	2
PLAY_SEQ	.equ	3
INPUT_SEQ	.equ	4
OVER		.equ	5


;-----------------------------------------------------------------------
			.text								; Program Start
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;	Functions
;-----------------------------------------------------------------------

;---------------------------
; delayMs
; delay = R15
;---------------------------
delayMs		clr.w	&time						; Reset time
$1			cmp.w	R15, &time					; If time < R15
			jl		$1							; continue delaying
			ret									; Return
			.newblock							; Undefine local labels

dly			.macro	delay
			mov.w	delay, R15
			call	#delayMs
			.endm

;---------------------------
; randGet (X ABC)
; rand = R15
; Remember to initialize
; randA-C before calling
;---------------------------
randGet		inc.w	&randX						; > x++             <
												; > a = (a^c^x)     <
			mov.w	&randC, R15					; c -> R15
			xor.w	&randX, R15					; R15 -> c^x
			xor.w	R15, &randA					; a -> c^x^a
			add.w	&randA, &randB				; > b = (b+a)       <
												; > c = (c+(b>>1)^a)<
			mov.w	&randB, R15					; b -> R15
			rra.w	R15							; R15 -> (b>>1)
			xor.w	&randA, R15					; R15 -> (b>>1)^a
			add.w	R15, &randC					; c -> (c+(b>>1)^a)
			mov.w	&randC, R15					; Move random to R15
			ret

;---------------------------
; btnRead
; btn = R15
;---------------------------
btnRead		clr.w	&P2DIR						; Configure P2 Inputs
			mov.w	#3, R15						; Set iterator/counter
			mov.w	#8, R14						; Put 1 to be rolled
$1			bit.w	R14, &P2IN					; Test
			jnz		$2							; If pushed return
			dec.w	R15							; Decrease counter
			rra.w	R14							; Roll to next button
			jnz		$1							; If zero, no button pushed
$2			inv.w	&P2DIR						; Configure P2 Outputs
			ret									; Return
			.newblock

;---------------------------
; LED Functions
; Uses R15
;---------------------------
_led		.byte	BIT4, BIT5, BIT6, BIT7		; Look-up values for leds

ledOn		.macro	LED							; ledOn		LED
			mov.b	LED, R15					; Move to register
			bic.b	_led(R15), &P1OUT			; Turn on led
			.endm

ledOff		.macro	LED							; ledOff	LED
			mov.b	LED, R15					; Move to register
			bis.b	_led(R15), &P1OUT			; Turn of led
			.endm

ledTgl		.macro	LED							; ledOn		LED
			mov.b	LED, R15					; Move to register
			xor.b	_led(R15), &P1OUT			; Turn on led
			.endm

;---------------------------
; seqGenerate
; Stores 32 random values
; on seq array (0-3)
;---------------------------
seqGenerate	clr.w	R14							; Iterator
$1			call	#randGet					; Random value -> R15
			and.b	#0x03, R15					; Truncate R14 (0-3)
			mov.b	R15, seq(R14)				; Copy value into array
			inc.w	R14							; Increment Iterator
			cmp.w	#32, R14					; Loop 32 times
			jne		$1							; If not 32, loop again
			ret									; Return
			.newblock							; Undefine local labels

;-----------------------------------------------------------------------
;	LCD Driver
;-----------------------------------------------------------------------

EN			.equ	BIT4
RS			.equ	BIT5
DATA		.equ	0x0F
CLEAR		.equ	0x01

;---------------------------
; lcdSendNib
; Takes nibble on R14 and
; sends it to the LCD
;---------------------------
lcdSendNib	bic.b	#DATA, &P2OUT				; Clear Data Pins
			and.b	#DATA, R14					; Mask lower nibble
			bis.b	R14, &P2OUT					; Send Nibble
			bis.b	#EN, &P2OUT					; Trigger Enable
			bic.b	#EN, &P2OUT					;
			ret									; Return

;---------------------------
; lcdWrite
; Writes data/cmd on R15 to
; LCD
;---------------------------
lcdWrite	mov.b	R15, R14					; Send upper nibble
			rra.b	R14							; R15 >> 4
			rra.b	R14							;
			rra.b	R14							;
			rra.b	R14							;
			call	#lcdSendNib					; Send
			mov.b	R15, R14					; Send lower nibble
			call 	#lcdSendNib					; Send
			ret									; Return

lcdWD		.macro	data						; Write Data to LCD
			bis.b	#RS, &P2OUT					; Set RS to Data Mode
			mov.b	data, R15					; Move data to R15
			call	#lcdWrite					; Write
			dly		#1							; Delay 1 ms
			.endm

lcdWC		.macro	cmd							; Write Cmd to LCD
			bic.b	#RS, &P2OUT					; Set RS to Cmd Mode
			mov.b	cmd, R15					; Move cmd to R15
			call	#lcdWrite					; Write
			dly		#5							; Delay 5 ms
			.endm

;---------------------------
; lcdWriteText
; Modifies the buffer array
; to be displayed.
; R14 -> x
; R13 -> string ptr
;---------------------------
lcdWriteText tst.b	0(R13)						; Test character
			jz		$1							; If null, string ends
			mov.b	@R13+, lcdBuffer(R14)		; char -> buffer
			inc.b	R14							; Increment buffer ptr
			jmp		lcdWriteText				; Next character
$1			call	#lcdUpdate					; Update LCD
			ret									; Return
			.newblock							; Undefine local labels

lcdWT		.macro	str, x						; Write text to LCD
			mov.w	str, R13					; Move into registers
			mov.w	x, R14						;
			call	#lcdWriteText				;
			.endm								;

;---------------------------
; lcdWriteInt (unsigned)
; R14 -> x (str pointer)
; R13 -> input integer
; R12 -> digit
; R11 -> iterator (stack)
;---------------------------
_test		.word	10d, 100d, 1000d, 10000d
lcdWriteInt	push	R11							; Register not reserved
			mov.w	#4, R11						; Do 4 iterations

$1			dec.w	R11							; Decrement iterator
			mov.w	#0x30, R12					; New digit '0'
			rla.w	R11							; Multiply iterator (*2)

$2			cmp.w	_test(R11), R13				; if input >= test, cont
			jl		$3							; Else, jump out
			inc.w	R12							; Increment digit
			sub.w	_test(R11), R13				; Subtract test from int
			jmp		$2							; Continue digit loop

$3			rra.w	R11							; Restore iterator (/2)
			mov.b	R12, lcdBuffer(R14)			; Move digit into buffer
			inc.w	R14							; Increment str pointer
			tst.w	R11							; Continue loop
			jnz		$1							; Until zero

			add.b	#0x30, R13					; Add '0' to char
			mov.b	R13, lcdBuffer(R14)			; Copy last digit
			pop		R11							; Restore register
			call	#lcdUpdate					; Update LCD
			ret									; Return
			.newblock							; Undefine local labels

lcdWI		.macro	int, x						; Write int to LCD
			mov.w	int, R13					;
			mov.w	x, R14						;
			call	#lcdWriteInt				;
			.endm								;


;---------------------------
; lcdUpdate
; Send lcdBuffer to the
; display
;---------------------------
lcdUpdate	clr.b	R12							; Clear pointer
			lcdWC	#0x80						; Go home
$1			lcdWD	lcdBuffer(R12)				; Write character
			inc.b	R12							; Increment pointer
			cmp		#16, R12					; Send first line
			jne		$1							; Loop
			lcdWC	#0xC0						; Go home
$2			lcdWD	lcdBuffer(R12)				; Write character
			inc.b	R12							; Increment pointer
			cmp		#32, R12					; Send first line
			jne		$2							; Loop
			ret									; Return
			.newblock							; Undefine local labels



;---------------------------
; lcdInit
; Initializes LCD
;---------------------------
lcdInit		dly		#100						; Delay 100 ms for power
			mov.b	#0xFF, &P2DIR				; Set P2 to outputs
			mov.b	#3, R11						; Iterator
$1			lcdWC	#0x03						; Change to 4-bit mode
			dly		#5							; Wait 5 ms
			dec.b	R11
			tst.b	R11							; If not 0
			jnz		$1							; Loop
			lcdWC	#0x02						; Really change to 4-bit
			dly 	#5							; Wait 5 ms
			lcdWC	#0x28						; 4-bit, 2 line, 5x8
			lcdWC	#0x08						; Instruction Flow
			lcdWC	#0x01						; Clear LCD
			lcdWC	#0x06						; Auto-Increment
			lcdWC	#0x0C						; Display On, No blink
			ret									; Return
			.newblock							; Undefine local labels

;-----------------------------------------------------------------------
;	Main Programs
;-----------------------------------------------------------------------
Init		mov.w	#0x0280, SP					; Initialize Stack
			mov.w	#WDTPW+WDTHOLD, &WDTCTL		; Disable Watch Dog
SetupP1		bis.b	#0xF0, &P1OUT				; Turn off leds
			bis.b	#0xF0, &P1DIR				; Enable outputs
SetupTA		mov.w	#TASSEL_2+MC_1, &TACTL		; SMCLK, up mode
			mov.w	#CCIE, &TACCTL0				; Compare interrupt
			mov.w	#1000, &TACCR0				; 1ms interrupt
			clr.w	&time						; Reset time
			eint								; Enable interrupts
SetupLCD	call	#lcdInit					; Initialize LCD
;-----------------------------------------------------------------------
InitVars	mov.w	#RESET, &state				; Set RESET state
;-----------------------------------------------------------------------
gameLoop	mov.w	&state, R13					; R13 -> state

			tst.w	R13							; reset
			jeq		reset						;

			dec.w	R13							; skillLevel
			jeq		skillLevel					;

			dec.w	R13							; genSeq
			jeq		genSeq						;

			dec.w	R13							; playSeq
			jeq		playSeq						;

			dec.w	R13							; inputSeq
			jeq		inputSeq					;

			dec.w	R13							; over
			jeq		over						;

			jmp		gameLoop					; gameLoop
;-----------------------------------------------------------------------
reset		lcdWT	#_welcome, #0				; Display Welcome
			dly		#2000						; Wait 2s
			mov.w	&TAR, &randA				; Move first seed
			mov.w	#SKILL_LEVEL, &state		; Switch to Skill Level
			jmp		gameLoop					; Back to gameLoop
;-----------------------------------------------------------------------
skillLevel	lcdWT	#_skilllvl, #0				; Display Skill Screen
$1			call	#btnRead					; Check button
			mov.w	R15, R10					; btn -> R15
			cmp.w	#-1, R10					; If no button pushed,
			jeq		$1							; check again
			rla.w	R10							; Multiply by 2 ptr
			lcdWT	_skString(R10), #16			; Show Skill Selected
			mov.w	_skOnTime(R10), &ledOnTime	; Set ledOnTime
			mov.w	_skOffTime(R10), &ledOffTime	; Set ledOffTime
			mov.w	_skBtnTime(R10), &btnTime	; Set btnTime
			mov.w	&TAR, &randB				; Move second seed
			dly		#2000						; Wait 2s
			mov.w	#GEN_SEQ, &state			; Switch to Gen Sequence
			jmp		gameLoop					; Back to gameLoop
			.newblock							; Undefine local labels
;-----------------------------------------------------------------------
genSeq		call	#seqGenerate				; Generate new sequence
			clr.w	&seqTop						; Initialize variables
			clr.w	&seqPos						;
			clr.w	&score						;
			lcdWT	#_score, #0					; Prepare screen
			mov.w	#PLAY_SEQ, &state			; Switch to Play Sequence
			jmp		gameLoop					; Back to gameLoop
;-----------------------------------------------------------------------
playSeq		cmp.w	&seqTop, &seqPos			; If seqPos > seqTop,
			jeq		notover?					; sequence is over.
			jge		over?						;

notover?	mov.w	&seqPos, R10				; seqPos -> R10
			ledOn	seq(R10)					; Blink LED
			dly		&ledOnTime					;
			ledOff	seq(R10)					;
			dly		&ledOffTime					;
			inc.w	&seqPos						; Advance to next position
			jmp		gameLoop					; Back to gameLoop

over?		clr.w	&seqPos						; Clear position and
			mov.w	#INPUT_SEQ, &state			; change state
			jmp		gameLoop					; Back to gameLoop
			.newblock							; Undefine local labels
;-----------------------------------------------------------------------
inputSeq	cmp.w	&seqTop, &seqPos			; If seqPos > seqTop,
			jeq		notover?					; sequence is over.
			jge		over?						;

notover?	mov.w	&seqPos, R10				; seqPos -> R10
			clr.w	&time						; Reset time
while?		cmp.w	&btnTime, &time				; while (time < btnTime)
			jge		lose?						; if (time > btnTime), loses
			call	#btnRead					; Read button
			mov.w	R15, R11					; btn -> R11
			cmp.w	#-1, R15					; If button is pressed, break
			jne		continue?
			jmp		while?

continue?	call	#btnRead					; btn -> R15
			cmp.w	#-1, R15					; while button is pressed
			jne		continue?

			ledOn	R11							; Blink pushed led
			dly		#20
			ledOff	R11

			cmp.b	R11, seq(R10)				; if (btn != seq[seqPos]), wrong btn
			jeq		correct?
lose?		mov.w	#OVER, &state				; Game Over
			jmp		gameLoop

correct?	inc.w	&seqPos						; Correct button, continue
			add.w	#50, &score					; Increment score
			lcdWI	&score, #16					; Display score
			jmp		gameLoop					; Back to gameLoop

over?		inc.w	&seqTop						; Sequence complete, increment top
			clr.w	&seqPos						; Clear Position
			mov.w	#PLAY_SEQ, &state			; Change state
			dly		#400						; Delay before playing
			jmp 	gameLoop					; Back to gameLoop
			.newblock							; Undefine local labels
;-----------------------------------------------------------------------
over		lcdWT	#_end, #0					; Write end screen
			lcdWI	&seqTop, #11				; Display score
			lcdWI	&score, #27					; Display score
			xor.b	#-1, &P1OUT					; Flash All Leds
			dly		#100						;
			xor.b	#-1, &P1OUT					;
			dly		#100						;
			xor.b	#-1, &P1OUT					;
			dly		#100						;
			xor.b	#-1, &P1OUT					;
			dly		#3000						; Delay 3s
			mov.w	#RESET, &state				; Switch to Reset
			jmp		gameLoop					; Back to gameLoop
;-----------------------------------------------------------------------


;-----------------------------------------------------------------------
TA0_ISR;	Time Keeping Interrupt
;-----------------------------------------------------------------------
			inc.w	&time					; Increment time
			reti

;-----------------------------------------------------------------------
;			Interrupt Vectors
;-----------------------------------------------------------------------
			.sect	".reset"
			.short	Init
			.sect	".int09"
			.short	TA0_ISR
			.end

