;
; example.s
; Brad Smith (rainwarrior), 4/06/2014
; http://rainwarrior.ca
;
; This is intended as an introductory example to NES programming with ca65.
; It covers the basic use of background, sprites, and the controller.
; This does not demonstrate how to use sound.
;
; This is not intended as a ready-made game. It is only a very minimal
; playground to assist getting started in NES programming. The idea here is
; to get you past the most difficult parts of a minimal NES program setup
; so that you can experiment from an almost blank slate.
;
; To use your own graphics, replace the two 4k tile banks provided.
; They are named "background.chr" and "sprite.chr".
;
; The reset and nmi routines are provided as a simple working example of
; these things. Writing these from scratch is a more advanced topic, so they
; will not be fully explained here.
;
; Under "drawing utilities" are some very primitive routines for working
; with the NES graphics. See the "main" section for examples of how to use them.
;
; Finally at the bottom you will find the "main" section that provides
; a small example program. A cursor is shown. Pressing the d-pad will move
;   - pressing the d-pad will move the cursor around the screen
;   - pressing B will draw a tile to the screen
;   - pressing A will draw several tiles to the screen
;   - pressing SELECT will reset the background
;   - holding START will demonstrate scrolling
;
; Please note that this example code is intended to be simple, not necessarily
; efficient. I have tried to avoid optimization in favour of easier to understand code.
;
; You may notice some odd behaviour when using the A button around the edges of the screen.
; I will leave it as an exercise for the curious to understand what is going on.
;

;
; iNES header
;

.segment "HEADER"

INES_MAPPER = 4 ; 0 = NROM
INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $10 ; 16k PRG chunk count
.byte $01 ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

;
; CHR ROM
;

.segment "TILES"
.incbin "background.chr"
.incbin "sprite.chr"


;
; vectors placed at top 6 bytes of memory area
;

.segment "VECTORS"
.word nmi
.word reset
.word irq

;
; reset routine
;
PPU_CONTROL := $2000
PPU_MASK    := $2001
PPU_STATUS  := $2002
OAM_ADDRESS := $2003
OAM_DATA    := $2004
PPU_SCROLL  := $2005
PPU_ADDRESS := $2006
PPU_DATA    := $2007

OAM_DMA     := $4014

PULSE1_CONTROL      := $4000
PULSE1_SWEEP        := $4001
PULSE1_TIMER_LOW    := $4002
PULSE1_TIMER_HIGH   := $4003
PULSE2_CONTROL      := $4004
PULSE2_SWEEP        := $4005
PULSE2_TIMER_LOW    := $4006
PULSE2_TIMER_HIGH   := $4007
TRIANGLE_CONTROL    := $4008
TRIANGLE_TIMER_LOW  := $400A
TRIANGLE_TIMER_HIGH := $400B
NOISE_CONTROL       := $400C
NOISE_TIMER_LOW     := $400E
NOISE_TIMER_HIGH    := $400F
DMC_FREQUENCY       := $4010
DMC_RAW             := $4011
DMC_ADDRESS         := $4012
DMC_LENGTH          := $4013

SOUND_CONTROL       := $4015  ; Write-only
SOUND_STATUS        := $4015  ; Read-only
FRAME_COUNTER       := $4017  ; Write-only


MMC3_BANK_SELECT    := $8000  ; Write-only.
MMC3_BANK_DATA      := $8001  ; Write-only.
.segment "CODE"
reset:
	sei       ; mask interrupts
	lda #0
	sta $2000 ; disable NMI
	sta $2001 ; disable rendering
	sta $4015 ; disable APU sound
	sta $4010 ; disable DMC IRQ
	lda #$40
	sta $4017 ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; initialize stack
	; wait for first vblank
	bit $2002
	:
		bit $2002
		bpl :-
	    ; Init MMC3 CHR.
    LDX #$00
    STX MMC3_BANK_SELECT
    LDA #$00
    STA MMC3_BANK_DATA

    INX
    STX MMC3_BANK_SELECT
    LDA #$02
    STA MMC3_BANK_DATA

    INX
    STX MMC3_BANK_SELECT
    LDA #$04
    STA MMC3_BANK_DATA

    INX
    STX MMC3_BANK_SELECT
    LDA #$05
    STA MMC3_BANK_DATA

    INX
    STX MMC3_BANK_SELECT
    LDA #$06
    STA MMC3_BANK_DATA

    INX
    STX MMC3_BANK_SELECT
    LDA #$07
    STA MMC3_BANK_DATA

    ; Init MMC3 PRG.
    INX
    STX MMC3_BANK_SELECT
    LDA #$00
    STA MMC3_BANK_DATA

    INX
    STX MMC3_BANK_SELECT
    LDA #$01
    STA MMC3_BANK_DATA

    ; In case theres a that writes to a mapper register, set the bank register to update one of the CHR banks.
    LDX #$05
    STA MMC3_BANK_SELECT
	; clear all RAM to 0
	lda #0
	ldx #0
	:
		sta $0000, X
		sta $0100, X
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	; place all sprites offscreen at Y=255
	lda #255
	ldx #0
	:
		sta oam, X
		inx
		inx
		inx
		inx
		bne :-
	; wait for second vblank
	:
		bit $2002
		bpl :-
	; NES is initialized, ready to begin!
	; enable the NMI for graphical updates, and jump to our main program
	lda #%10001000
	sta $2000
	jmp main
	nop
	rts
	

;
; nmi routine
;

.segment "ZEROPAGE"
nmi_lock:       .res 1 ; prevents NMI re-entry
nmi_count:      .res 1 ; is incremented every NMI
nmi_ready:      .res 1 ; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
nmt_update_len: .res 1 ; number of bytes in nmt_update buffer
scroll_x:       .res 1 ; x scroll position
scroll_y:       .res 1 ; y scroll position
scroll_nmt:     .res 1 ; nametable select (0-3 = $2000,$2400,$2800,$2C00)
temp:           .res 1 ; temporary variable

.segment "BSS"
nmt_update: .res 256 ; nametable update entry buffer for PPU update
palette:    .res 32  ; palette buffer for PPU update

.segment "OAM"
oam: .res 256        ; sprite OAM data to be uploaded by DMA

.segment "CODE"


.macro branch_check opc, dest
  opc dest
  .assert >* = >(dest), warning, "branch_check: failed, crosses page"
.endmacro

.macro Jcc dest
  branch_check bcc, dest
.endmacro
.macro Jcs dest
  branch_check bcs, dest
.endmacro
.macro Jeq dest
  branch_check beq, dest
.endmacro
.macro Jne dest
  branch_check bne, dest
.endmacro
.macro Jmi dest
  branch_check bmi, dest
.endmacro
.macro Jpl dest
  branch_check bpl, dest
.endmacro
.macro Jvc dest
  branch_check bvc, dest
.endmacro
.macro Jvs dest
  branch_check bvs, dest
.endmacro

nmi:
	; save registers
	pha
	txa
	pha
	tya
	pha
	; prevent NMI re-entry
	lda nmi_lock
	beq :+
		jmp @nmi_end
	:
	lda #1
	sta nmi_lock
	; increment frame counter
	inc nmi_count
	;
	lda nmi_ready
	bne :+ ; nmi_ready == 0 not ready to update PPU
		jmp @ppu_update_end
	:
	cmp #2 ; nmi_ready == 2 turns rendering off
	bne :+
		lda #%00000000
		sta $2001
		ldx #0
		stx nmi_ready
		jmp @ppu_update_end
	:
	; sprite OAM DMA
	ldx #0
	stx $2003
	lda #>oam
	sta $4014
	; palettes
	lda #%10001000
	sta $2000 ; set horizontal nametable increment
	lda $2002
	lda #$3F
	sta $2006
	stx $2006 ; set PPU address to $3F00
	ldx #0
	:
		lda palette, X
		sta $2007
		inx
		cpx #32
		bcc :-
	; nametable update
	ldx #0
	cpx nmt_update_len
	bcs @scroll
	@nmt_update_loop:
		lda nmt_update, X
		sta $2006
		inx
		lda nmt_update, X
		sta $2006
		inx
		lda nmt_update, X
		sta $2007
		inx
		cpx nmt_update_len
		bcc @nmt_update_loop
	lda #0
	sta nmt_update_len
@scroll:
	lda scroll_nmt
	and #%00000011 ; keep only lowest 2 bits to prevent error
	ora #%10001000
	sta $2000
	lda scroll_x
	sta $2005
	lda scroll_y
	sta $2005
	; enable rendering
	lda #%00011110
	sta $2001
	; flag PPU update complete
	ldx #0
	stx nmi_ready
@ppu_update_end:
	; if this engine had music/sound, this would be a good place to play it
	; unlock re-entry flag
	lda #0
	sta nmi_lock
@nmi_end:
	; restore registers and return
	pla
	tay
	pla
	tax
	pla
	rti

;
; irq
;

.segment "CODE"
irq:
	rti

;
; drawing utilities
;

.segment "CODE"

; ppu_update: waits until next NMI, turns rendering on (if not already), uploads OAM, palette, and nametable update to PPU
ppu_update:
	lda #1
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

; ppu_skip: waits until next NMI, does not update PPU
ppu_skip:
	lda nmi_count
	:
		cmp nmi_count
		beq :-
	rts

; ppu_off: waits until next NMI, turns rendering off (now safe to write PPU directly via $2007)
ppu_off:
	lda #2
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

; ppu_address_tile: use with rendering off, sets memory address to tile at X/Y, ready for a $2007 write
;   Y =  0- 31 nametable $2000
;   Y = 32- 63 nametable $2400
;   Y = 64- 95 nametable $2800
;   Y = 96-127 nametable $2C00
ppu_address_tile:
	lda $2002 ; reset latch
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta $2006
	tya
	asl
	asl
	asl
	asl
	asl
	sta temp
	txa
	ora temp
	sta $2006 ; low bits of Y + X
	rts

; ppu_update_tile: can be used with rendering on, sets the tile at X/Y to tile A next time you call ppu_update
ppu_update_tile:
	pha ; temporarily store A on stack
	txa
	pha ; temporarily store X on stack
	ldx nmt_update_len
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta nmt_update, X
	inx
	tya
	asl
	asl
	asl
	asl
	asl
	sta temp
	pla ; recover X value (but put in A)
	ora temp
	sta nmt_update, X
	inx
	pla ; recover A value (tile)
	sta nmt_update, X
	inx
	stx nmt_update_len
	rts

; ppu_update_byte: like ppu_update_tile, but X/Y makes the high/low bytes of the PPU address to write
;    this may be useful for updating attribute tiles
ppu_update_byte:
	pha ; temporarily store A on stack
	tya
	pha ; temporarily store Y on stack
	ldy nmt_update_len
	txa
	sta nmt_update, Y
	iny
	pla ; recover Y value (but put in Y)
	sta nmt_update, Y
	iny
	pla ; recover A value (byte)
	sta nmt_update, Y
	iny
	sty nmt_update_len
	rts

;
; gamepad
;

PAD_A      = $01
PAD_B      = $02
PAD_SELECT = $04
PAD_START  = $08
PAD_U      = $10
PAD_D      = $20
PAD_L      = $40
PAD_R      = $80

.segment "ZEROPAGE"
gamepad: .res 1

.segment "CODE"
; gamepad_poll: this reads the gamepad state into the variable labelled "gamepad"
;   This only reads the first gamepad, and also if DPCM samples are played they can
;   conflict with gamepad reading, which may give incorrect results.
gamepad_poll:
	; strobe the gamepad to latch current button state
	lda #1
	sta $4016
	lda #0
	sta $4016
	; read 8 bytes from the interface at $4016
	ldx #8
	:
		pha
		lda $4016
		; combine low two bits and store in carry bit
		and #%00000011
		cmp #%00000001
		pla
		; rotate carry into gamepad variable
		ror
		dex
		bne :-
	sta gamepad
	nop
	rts

;
; main
;

 
.segment "RODATA"
example_palette:
.byte $0F,$15,$26,$37 ; bg0 purple/pink
.byte $0F,$09,$19,$29 ; bg1 green
.byte $0F,$01,$11,$21 ; bg2 blue
.byte $0F,$00,$10,$30 ; bg3 greyscale
.byte $0F,$18,$28,$38 ; sp0 yellow
.byte $0F,$14,$24,$34 ; sp1 purple
.byte $0F,$1B,$2B,$3B ; sp2 teal
.byte $0F,$12,$22,$32 ; sp3 marine

.segment "ZEROPAGE"
cursor_x: .res 1
cursor_y: .res 1
temp_x:   .res 1
temp_y:   .res 1

.segment "CODE"

main:
	; setup 
	ldx #0
	:
		lda example_palette, X
		sta palette, X
		inx
		cpx #32
		bcc :-
	jsr setup_background
	; center the cursor
	lda #128
	sta cursor_x
	lda #120
	sta cursor_y
	; show the screen
	jsr draw_cursor
	jsr ppu_update
	jsr startdelay	
	LDA #$80
    LDY #$29
    jsr sendtoYM
	jmp $8000
	; start of VGM

.segment "CODE"
	; end of VGM
	; main loop
main_loop:
@loop:
	; read gamepad
	jsr gamepad_poll
	; respond to gamepad state
	lda gamepad
	and #PAD_START
	beq :+
		jsr push_start
		jmp @draw ; start trumps everything, dont check other buttons
	:
	jsr release_start ; releasing start restores scroll
	lda gamepad
	and #PAD_U
	beq :+
		jsr push_u
	:
	lda gamepad
	and #PAD_D
	beq :+
		jsr push_d
	:
	lda gamepad
	and #PAD_L
	beq :+
		jsr push_l
	:
	lda gamepad
	and #PAD_R
	beq :+
		jsr push_r
	:
	lda gamepad
	and #PAD_SELECT
	beq :+
		jsr push_select
	:
	lda gamepad
	and #PAD_B
	beq :+
		jsr push_b
	:
	lda gamepad
	and #PAD_A
	beq :+
		jsr push_a
	:
@draw:
	; draw everything and finish the frame
	jsr draw_cursor
	jsr ppu_update
	; keep doing this forever!
	jmp @loop

;  The delay is 9*(256*A+Y)+8 cycles (plus 12 more for JSR & RTS if you make it a subroutine)
delayloop1:
	lda #0
	ldy #2
	nop
	nop
	jmp delayloop
delayloop2:
	lda #0
	ldy #7
	jmp delayloop
delayloop3:
	lda #0
	ldy #11
	jmp delayloop
delayloop4:
	lda #0
	ldy #16
	jmp delayloop
delayloop5:
	lda #0
	ldy #21
	jmp delayloop
delayloop6:
	lda #0
	ldy #25
	jmp delayloop
delayloop7:
	lda #0
	ldy #30
	jmp delayloop
delayloop8:
	lda #0
	ldy #34
	jmp delayloop
delayloop9:
	lda #0
	ldy #39
	jmp delayloop
delayloop10:
	lda #0
	ldy #43
	jmp delayloop
delayloop11:
	lda #0
	ldy #48
	jmp delayloop
delayloop12:
	lda #0
	ldy #52
	jmp delayloop
delayloop13:
	lda #0
	ldy #57
	jmp delayloop
delayloop14:
	lda #0
	ldy #62
	jmp delayloop
delayloop15:
	lda #0
	ldy #66
	jmp delayloop
delayloop16:
	lda #0
	ldy #71
	jmp delayloop
delayloop882:
	lda #15
	ldy #5
	jmp delayloop	
delayloop735:
	lda #13
	ldy #2
	jmp delayloop	

startdelay:
		lda #250
		;sta $255
		ldy #0
		;sty $255
delayloop:   CPY  #1
        DEY
        SBC  #0
        BCS  delayloop
		rts
		
		
sendtoYM:
	nop
	STA $C000 ;3 cycles
	nop ;2 cycles
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	STY $E000
	ldy #31
	lda #0
sendtoYMloop:	
	CPY  #1
	DEY
    SBC  #0
    BCS  sendtoYMloop
	rts

sendtoYM2:
	nop
	nop
	STA $C002 
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	STY $E002
	ldy #31
	lda #0
sendtoYM2loop:	
	CPY  #1
	DEY
    SBC  #0
    BCS  sendtoYM2loop
	rts		
	
sendtoYMLowDelay:
	nop
	nop
	nop
	STA $C000 
	nop
	nop
	nop
	STY $E000
	nop
	nop
	rts

sendtoYM2LowDelay:
	nop
	nop
	nop
	STA $C002 
	nop
	nop
	nop
	STY $E002
	nop
	nop
	rts
	
push_u:
	dec cursor_y
	; Y wraps at 240
	lda cursor_y
	cmp #240
	bcc :+
		lda #239
		sta cursor_y
	:
	rts

push_d:
	inc cursor_y
	; Y wraps at 240
	lda cursor_y
	cmp #240
	bcc :+
		lda #0
		sta cursor_y
	:
	rts

push_l:
	dec cursor_x
	rts

push_r:
	inc cursor_x
	rts

push_select:
	; turn off rendering so we can manually update entire nametable
	jsr ppu_off
	jsr setup_background
	; wait for user to release select before continuing
	:
		jsr gamepad_poll
		lda gamepad
		and #PAD_SELECT
		bne :-
	rts

push_start:
	inc scroll_x
	inc scroll_y
	; Y wraps at 240
	lda scroll_y
	cmp #240
	bcc :+
		lda #0
		sta scroll_y
	:
	; when X rolls over, toggle the high bit of nametable select
	lda scroll_x
	bne :+
		lda scroll_nmt
		eor #$01
		sta scroll_nmt
	:
	rts

release_start:
	lda #0
	sta scroll_x
	sta scroll_y
	sta scroll_nmt
	rts

push_b:
	jsr snap_cursor
	lda cursor_x
	lsr
	lsr
	lsr
	tax ; X = cursor_x / 8
	lda cursor_y
	lsr
	lsr
	lsr
	tay ; Y = cursor_y / 8
	lda #4
	jsr ppu_update_tile ; puts tile 4 at X/Y
	rts

push_a:
	jsr snap_cursor
	lda cursor_x
	lsr
	lsr
	lsr
	sta temp_x ; cursor_x / 8
	lda cursor_y
	lsr
	lsr
	lsr
	sta temp_y ; cursor_y / 8
	; draw a ring of 8 tiles around the cursor
	dec temp_x ; x-1
	dec temp_y ; y-1
	ldx temp_x
	ldy temp_y
	lda #5
	jsr ppu_update_tile
	inc temp_x ; x
	ldx temp_x
	ldy temp_y
	lda #6
	jsr ppu_update_tile
	inc temp_x ; x+1
	ldx temp_x
	ldy temp_y
	lda #5
	jsr ppu_update_tile
	dec temp_x
	dec temp_x ; x-1
	inc temp_y ; y
	ldx temp_x
	ldy temp_y
	lda #6
	jsr ppu_update_tile
	inc temp_x
	inc temp_x ; x+1
	ldx temp_x
	ldy temp_y
	lda #6
	jsr ppu_update_tile
	dec temp_x
	dec temp_x ; x-1
	inc temp_y ; y+1
	ldx temp_x
	ldy temp_y
	lda #5
	jsr ppu_update_tile
	inc temp_x ; x
	ldx temp_x
	ldy temp_y
	lda #6
	jsr ppu_update_tile
	inc temp_x ; x+1
	ldx temp_x
	ldy temp_y
	lda #5
	jsr ppu_update_tile
	rts

; snap_cursor: snap cursor to nearest tile
snap_cursor:
	lda cursor_x
	clc
	adc #4
	and #$F8
	sta cursor_x
	lda cursor_y
	clc
	adc #4
	and #$F8
	sta cursor_y
	; Y wraps at 240
	cmp #240
	bcc :+
		lda #0
		sta cursor_y
	:
	rts

draw_cursor:
	; four sprites centred around the currently selected tile
	; y position (note, needs to be one line higher than sprites appearance)
	lda cursor_y
	sec
	sbc #5 ; Y-5
	sta oam+(0*4)+0
	sta oam+(1*4)+0
	lda cursor_y
	clc
	adc #3 ; Y+3
	sta oam+(2*4)+0
	sta oam+(3*4)+0
	; tile
	lda #1
	sta oam+(0*4)+1
	sta oam+(1*4)+1
	sta oam+(2*4)+1
	sta oam+(3*4)+1
	; attributes
	lda #%00000000 ; no flip
	sta oam+(0*4)+2
	lda #%01000000 ; horizontal flip
	sta oam+(1*4)+2
	lda #%10000000 ; vertical flip
	sta oam+(2*4)+2
	lda #%11000000 ; both flip
	sta oam+(3*4)+2
	; x position
	lda cursor_x
	sec
	sbc #4 ; X-4
	sta oam+(0*4)+3
	sta oam+(2*4)+3
	lda cursor_x
	clc
	adc #4 ; X+4
	sta oam+(1*4)+3
	sta oam+(3*4)+3
	rts

setup_background:
	; first nametable, start by clearing to empty
	lda $2002 ; reset latch
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	; empty nametable
	lda #0
	ldy #30 ; 30 rows
	:
		ldx #32 ; 32 columns
		:
			sta $2007
			dex
			bne :-
		dey
		bne :--
	; set all attributes to 0
	ldx #64 ; 64 bytes
	:
		sta $2007
		dex
		bne :-
	; fill in an area in the middle with 1/2 checkerboard
	lda #1
	ldy #8 ; start at row 8
	:
		pha ; temporarily store A, it will be clobbered by ppu_address_tile routine
		ldx #8 ; start at column 8
		jsr ppu_address_tile
		pla ; recover A
		; write a line of checkerboard
		ldx #8
		:
			sta $2007
			eor #$3
			inx
			cpx #(32-8)
			bcc :-
		eor #$3
		iny
		cpy #(30-8)
		bcc :--
	; second nametable, fill with simple pattern
	lda #$24
	sta $2006
	lda #$00
	sta $2006
	lda #$00
	ldy #30
	:
		ldx #32
		:
			sta $2007
			clc
			adc #1
			and #3
			dex
			bne :-
		clc
		adc #1
		and #3
		dey
		bne :--
	; 4 stripes of attribute
	lda #0
	ldy #4
	:
		ldx #16
		:
			sta $2007
			dex
			bne :-
		clc
		adc #%01010101
		dey
		bne :--
	rts

;
; end of file
;


.segment "BANK00"

;end