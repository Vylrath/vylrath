
; ------------------------------------------------------------------------------
; Constant declarations
; ------------------------------------------------------------------------------
PPU_CONTROL = $2000        ; PPU control register 1 (write)
PPU_MASK = $2001           ; PPU control register 2 (write)
PPU_STATUS = $2002         ; PPU status register (read)
PPU_SPRRAM_ADDRESS = $2003 ; PPU sprite RAM address (write)
PPU_VRAM_ADDRESS2 = $2006  ; PPU VRAM address (write)
PPU_VRAM_IO = $2007        ; PPU VRAM data (read/write)
SPRITE_DMA = $4014         ; PPU sprite DMA (write)

APU_DM_CONTROL = $4010 ; APU DMC control (write)

JOYPAD1 = $4016 ; Joypad 1 (read/write)
JOYPAD2 = $4017 ; Joypad 2 (read/write)

PAD_A = %00000001      ; Joypad A button
PAD_B = %00000010      ; Joypad B button
PAD_SELECT = %00000100 ; Joypad select button
PAD_START = %00001000  ; Joypad start button
PAD_UP = %00010000     ; Joypad up button
PAD_DOWN = %00100000   ; Joypad down button
PAD_LEFT = %01000000   ; Joypad left button
PAD_RIGHT = %10000000  ; Joypad right button
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; iNES file/header format
; ------------------------------------------------------------------------------
.segment "HEADER"
; ------------------------------------------------------------------------------
.byte $4E       ; Byte 00: ASCII code for 'N'
.byte $45       ; Byte 01: ASCII code for 'E'
.byte $53       ; Byte 02: ASCII code for 'S'
.byte $1A       ; Byte 03: ASCII code for 'SUB'
.byte $02       ; Byte 04: PRG-ROM bank count in 16KB units
.byte $01       ; Byte 05: CHR-ROM bank count in 8KB units
.byte %00000000 ; Byte 06: Mapper, mirroring, battery, trainer:
                ;   4b - lower nybble of mapper number
                ;   1b - alternative nametable layout Boolean
                ;   1b - battery-backed PRG-RAM Boolean
                ;   1b - 512-byte trainer Boolean
                ;   1b - horizontal mirroring nametable Boolean
.byte %00000000 ; Byte 07: Mapper, NES 2.0, PlayChoice-10, VS Unisystem:
                ;   4b - upper nybble of mapper number
                ;   2b - if equal to '10' (2), use NES 2.0 format
                ;   1b - PlayChoice-10 Boolean
                ;   1b - VS Unisystem Boolean
.byte $00       ; Byte 08: PRG-RAM bank count in 8KB units
.byte %00000000 ; Byte 09: TV system:
                ;   7b - reserved
                ;   1b - PAL TV system Boolean
.byte %00000000 ; Byte 10: PRG-RAM present, TV system
                ;   2b - unused
                ;   1b - Board has bus conflicts Boolean
                ;   1b - PRG-RAM present Boolean
                ;   2b - unused
                ;   2b - TV system (0: NTSC, 2: PAL, 1/3: dual-compatible)
.byte $00       ; Byte 11: unused
.byte $00       ; Byte 12: unused
.byte $00       ; Byte 13: unused
.byte $00       ; Byte 14: unused
.byte $00       ; Byte 15: unused
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; PPU pattern tables (CHR-ROM) consisting of both background and sprite sets
; ------------------------------------------------------------------------------
.segment "TILES"
; ------------------------------------------------------------------------------
.incbin "vylrath.chr"
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; NES interrupt vectors
; ------------------------------------------------------------------------------
.segment "VECTORS"
; ------------------------------------------------------------------------------
.word nmi   ; Non-maskable intterupt vector
.word reset ; Reset vector
.word irq   ; Interrupt request vector
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Zero page memory (256b)
; ------------------------------------------------------------------------------
.segment "ZEROPAGE"
; ------------------------------------------------------------------------------
nmi_ready: .res 1 ; NMI flag
                  ; Set to 1 to push a PPU frame update
                  ; Set to 2 to turn rendering off for the next NMI

gamepad: .res 1 ; Gamepad state
                ;   Bit 0: A button
                ;   Bit 1: B button
                ;   Bit 2: Select button
                ;   Bit 3: Start button
                ;   Bit 4: Up button
                ;   Bit 5: Down button
                ;   Bit 6: Left button
                ;   Bit 7: Right button

d_x: .res 1 ; x velocity of ball

d_y: .res 1 ; y velocity of ball
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Sprite object attribute memory (256B)
; ------------------------------------------------------------------------------
.segment "OAM"
; ------------------------------------------------------------------------------
oam: .res 256 ; OAM data
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Remainder of RAM area for variable storage
; ------------------------------------------------------------------------------
.segment "BSS"
; ------------------------------------------------------------------------------
palette: .res 32 ; PPU palette buffer
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Main application entry point; called on startup and reset
; ------------------------------------------------------------------------------
.segment "CODE"
; ------------------------------------------------------------------------------
.proc ppu_update
    lda #$01
    sta nmi_ready
    loop:
        lda nmi_ready ; Set to 1 to push a PPU frame update
        bne loop
    rts
.endproc

.proc ppu_off
    lda #$02
    sta nmi_ready
    loop:
        lda nmi_ready ; Set to 2 to turn rendering off for the next NMI
        bne loop
    rts
.endproc
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Main application entry point; called on startup and reset
; ------------------------------------------------------------------------------
.segment "CODE"
; ------------------------------------------------------------------------------
.proc reset
    sei ; Set interrupt disable
    cld ; Clear decimal mode

    ldx #$40
    stx JOYPAD2 ; Disable joypad 2

    ldx #$FF ; Set the stack pointer to the top of the stack
    txs      ; Transfer x to the stack pointer

    inx ; Wrap x back to 0

    stx PPU_CONTROL    ; Disable NMI
    stx PPU_MASK       ; Disable rendering
    stx APU_DM_CONTROL ; Disable APU 

    bit PPU_STATUS ; Clear PPU status bits in memory with accumulator

wait_vblank1:
    bit PPU_STATUS   ; Test PPU status bits in memory with accumulator
    bpl wait_vblank1 ; Loop while the accumulator is positive

    lda #$00 ; RAM initialization value (0)
    ldx #$00 ; Starts the x register at 0 to increment to 255

clear_ram:
    ; Set the RAM to the value 0 (A register) from addresses $0000 to $07FF
    sta $0000, x
    sta $0100, x
    sta $0200, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    inx
    bne clear_ram ; Once X is back to 0, the zero flag is set and the loop exits

    lda #$FF ; Place all sprites offscreen at Y = 255
    ldx #$00

clear_oam:
	sta oam, x
	inx
	inx
	inx
	inx
	bne clear_oam

wait_vblank2:
    bit PPU_STATUS   ; Test PPU status bits in memory with accumulator
    bpl wait_vblank2 ; Loop while the accumulator is positive

    lda #%10001000  ; 1b - NMI enable
                    ; 1b - PPU master/slave select
                    ; 1b - 8x8/8x16 sprite size
                    ; 1b - Background pattern table address
                    ; 1b - Sprite pattern table address
                    ; 1b - PPU address increment
                    ; 2b - Name table select
    sta PPU_CONTROL ; Enable NMI for graphical updates

    jmp main ; Start the main loop
.endproc
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; NMI routine, called every vertical blank
; ------------------------------------------------------------------------------
.segment "CODE"
; ------------------------------------------------------------------------------
.proc nmi
    ; Save registers to stack for later restoration
    pha ; Push accumulator to stack
    txa ; Transfer x to accumulator
    pha ; Push accumulator to stack
    tya ; Transfer y to accumulator
    pha ; Push accumulator to stack

    lda nmi_ready
    bne :+ ; If nmi_ready is not 0, skip the next instruction
        jmp ppu_update_end ; Jump to the end of the NMI routine
    :

    cmp #$02               ; See if rendering should be turned off
    bne cont_render
        lda #%00000000     ; 1b - blue emphasis
                           ; 1b - green emphasis
                           ; 1b - red emphasis
                           ; 1b - sprite visibility
                           ; 1b - background visibility
                           ; 1b - sprite clipping
                           ; 1b - background clipping
        sta PPU_MASK       ; Disable rendering
        ldx #$00
        stx nmi_ready      ; Reset NMI ready flag to 0
        jmp ppu_update_end

cont_render:
    ldx #$00
    stx PPU_SPRRAM_ADDRESS ; Set the sprite RAM address to 0 and use OAMDMA
    lda #>oam              ; Load the high byte of the OAM address
    sta SPRITE_DMA         ; Transfer the sprite data to the PPU

    ; Transfer palette to PPU
    lda #%10001000        ; 1b - NMI enable
                          ; 1b - PPU master/slave select
                          ; 1b - 8x8/8x16 sprite size
                          ; 1b - Background pattern table address
                          ; 1b - Sprite pattern table address
                          ; 1b - PPU address increment
                          ; 2b - Name table select
    sta PPU_CONTROL       ; Enable NMI for graphical updates
    lda PPU_STATUS        ; Reset the PPU address latch
    lda #$3F              ; Set the palette address to $3F00
    sta PPU_VRAM_ADDRESS2 ; Set the PPU address to $3F00
    stx PPU_VRAM_ADDRESS2
    ldx #$00              ; Start at the beginning of the palette

loop:
    lda palette, x  ; Load the palette data
    sta PPU_VRAM_IO ; Transfer the palette data to the PPU
    inx             ; Increment the palette index
    cpx #$20        ; Check if the palette index is at the end
    bcc loop

    lda #%00011110 ; 1b - blue emphasis
                   ; 1b - green emphasis
                   ; 1b - red emphasis
                   ; 1b - sprite visibility
                   ; 1b - background visibility
                   ; 1b - sprite clipping
                   ; 1b - background clipping
    sta PPU_MASK   ; Enable rendering
    ldx #$00
    stx nmi_ready  ; Reset NMI ready flag

ppu_update_end:
    ; Restore registers from stack
    pla ; Pull accumulator from stack
    tay ; Transfer accumulator to y
    pla ; Pull accumulator from stack
    tax ; Transfer accumulator to x
    pla ; Pull accumulator from stack

    rti ; Return from interrupt
.endproc
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; IRQ clock interrupt routine
; ------------------------------------------------------------------------------
.segment "CODE"
; ------------------------------------------------------------------------------
.proc irq
    rti ; Return from interrupt
.endproc
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Main application logic and game loop
; ------------------------------------------------------------------------------
.segment "CODE"
; ------------------------------------------------------------------------------
.proc main
    ldx #$00

paletteloop:
    lda default_palette, x ; Load the default palette data
    sta palette, x         ; Transfer the default palette data to the PPU
    inx
    cpx #$20               ; Check if the palette index is at the end
    bcc paletteloop

    jsr clear_nametable ; Clear the nametable

    ; Draw some text on the screen
    lda PPU_STATUS        ; Reset the PPU address latch
    lda #$20              ; Set the PPU address to $208A (row 4. col 10) 
    sta PPU_VRAM_ADDRESS2
    lda #$8A
    sta PPU_VRAM_ADDRESS2

    ldx #$00 ; Start at the beginning of the text
textloop:
    lda hello, x    ; Load the text data
    sta PPU_VRAM_IO ; Transfer the text data to the PPU
    inx             ; Increment the text index
    cmp #$00        ; Check if the text is at the end
    beq :+          ; If the text is at the end, skip the next instruction
    jmp textloop    ; Continue drawing the text
    :               ; End of the text

    lda #$B4              ; Set the sprite Y position
    sta oam               ; Store the sprite Y position
    lda #$78              ; Set the sprite X position
    sta oam + 3           ; Store the sprite X position
    lda #$01              ; Set the sprite tile index
    sta oam + 1           ; Store the sprite tile index
    lda #$00              ; Set the sprite attributes
    sta oam + 2           ; Store the sprite attributes
    lda #$7C              ; Set the sprite Y position
    sta oam + (1 * 4)     ; Store the sprite Y position
    sta oam + (1 * 4) + 3 ; Store the sprite X position
    lda #$02              ; Set the sprite tile index
    sta oam + (1 * 4) + 1 ; Store the sprite tile index
    lda #$00              ; Set the sprite attributes
    sta oam + (1 * 4) + 2 ; Store the sprite attributes
    lda #$01              ; Set the x velocity of the ball
    sta d_x               ; Store the x velocity of the ball
    sta d_y               ; Store the y velocity of the ball

    jsr ppu_update  ; Push a PPU frame update

mainloop:
    lda nmi_ready ; Check if the NMI flag is set
    cmp #$00      ; If the NMI flag is not set, skip the next instruction
    bne mainloop  ; Continue the main loop

    jsr gamepad_poll ; Poll the gamepad

    lda gamepad               ; Load the gamepad state
    and #PAD_LEFT             ; Check if the left button is pressed
    beq NOT_GAMEPAD_LEFT      ; If the left button is not pressed, skip
        lda oam + 3           ; Load the sprite X position
        cmp #$00              ; Check if the sprite is at the left edge
        beq NOT_GAMEPAD_LEFT  ; If the sprite is at the left edge, skip
        sec                   ; Set the carry flag
        sbc #$01              ; Move the sprite left
        sta oam + 3           ; Store the sprite X position
NOT_GAMEPAD_LEFT:             ; End of the left button check
    lda gamepad               ; Load the gamepad state
    and #PAD_RIGHT            ; Check if the right button is pressed
    beq NOT_GAMEPAD_RIGHT     ; If the right button is not pressed, skip
        lda oam + 3           ; Load the sprite X position
        cmp #$F8              ; Check if the sprite is at the right edge
        beq NOT_GAMEPAD_RIGHT ; If the sprite is at the right edge, skip
        clc                   ; Clear the carry flag
        adc #$01              ; Move the sprite right
        sta oam + 3           ; Store the sprite X position
NOT_GAMEPAD_RIGHT:            ; End of the right button check
    lda oam + (1 * 4) + 0     ; Load the sprite Y position
    clc                       ; Clear the carry flag
    adc d_y                   ; Move the sprite down
    sta oam + (1 * 4) + 0     ; Store the sprite Y position
    cmp #$00                  ; Check if the sprite is at the top edge
    bne NOT_HITTOP            ; If the sprite is not at the top edge, skip
        lda #$01              ; Reverse direction
        sta d_y               ; Store the sprite Y position
NOT_HITTOP:                   ; End of the top edge check
    lda oam + (1 * 4) + 0     ; Load the sprite Y position
    cmp #$D2                  ; Check if the sprite is at the bottom edge
    bne NOT_HITBOTTOM         ; If the sprite is not at the bottom edge, skip
        lda #$FF              ; Reverse direction
        sta d_y               ; Store the sprite Y position
NOT_HITBOTTOM:                ; End of the bottom edge check
    lda oam + (1 * 4) + 3     ; Load the sprite tile index
    clc                       ; Clear the carry flag
    adc d_x                   ; Move the sprite right
    sta oam + (1 * 4) + 3     ; Store the sprite tile index
    cmp #$00                  ; Check if the sprite is at the left edge
    bne NOT_HITLEFT           ; If the sprite is not at the left edge, skip
        lda #$01              ; Reverse direction
        sta d_x               ; Store the sprite tile index
NOT_HITLEFT:
    lda oam + (1 * 4) + 3     ; Load the sprite tile index
    cmp #$F8                  ; Check if the sprite is at the right edge
    bne NOT_HITRIGHT          ; If the sprite is not at the right edge, skip
        lda #$FF              ; Reverse direction
        sta d_x               ; Store the sprite tile index
NOT_HITRIGHT:

    lda #$01      ; Set the NMI flag to 1
    sta nmi_ready ; Push a PPU frame update
    jmp mainloop  ; Continue the main loop
.endproc
; ------------------------------------------------------------------------------


; ------------------------------------------------------------------------------
; Clears nametable (30 rows by 32 columns)
; ------------------------------------------------------------------------------
.segment "CODE"
; ------------------------------------------------------------------------------
.proc clear_nametable
    lda PPU_STATUS        ; Reset the PPU address latch
    lda #$20              ; Set the PPU address to $2000
    sta PPU_VRAM_ADDRESS2
    lda #$00
    sta PPU_VRAM_ADDRESS2

    lda #$00
    ldy #$1E ; Clear 30 rows

    rowloop:
        ldx #$20            ; Clear 32 columns
        columnloop:
            sta PPU_VRAM_IO ; Clear the nametable (960B)
            dex
            bne columnloop
        dey
        bne rowloop

    ldx #$40 ; Clear the attribute table (64B)
    loop:
        sta PPU_VRAM_IO
        dex
        bne loop

    rts
.endproc
; ------------------------------------------------------------------------------


; ------------------------------------------------------------------------------
; Poll the game pad and store the state in the gamepad variable
; ------------------------------------------------------------------------------
.segment "CODE"
; ------------------------------------------------------------------------------
.proc gamepad_poll
    lda #$01
    sta JOYPAD1 ; Strobe the joypad 1 register
    lda #$00
    sta JOYPAD1 ; Reset the joypad 1 register
    ldx #$08    ; Start at the first button

loop:
    pha            ; Push the accumulator to the stack
    lda JOYPAD1    ; Load the joypad 1 register
    and #%00000011 ; Combine low two bits and store in carry bit
    cmp #%00000001
    pla            ; Restore the accumulator
    ror            ; Rotate the carry bit into the accumulator
    dex
    bne loop
    sta gamepad    ; Store the gamepad state
    rts
.endproc
; ------------------------------------------------------------------------------


; ------------------------------------------------------------------------------
; Read-only data
; ------------------------------------------------------------------------------
.segment "RODATA"
; ------------------------------------------------------------------------------
default_palette:
; Background palette data
.byte $0F, $00, $10, $30
.byte $0F, $01, $21, $31
.byte $0F, $06, $16, $26
.byte $0F, $09, $19, $29

; Sprite palette data
.byte $0F, $00, $10, $30
.byte $0F, $01, $21, $31
.byte $0F, $06, $16, $26
.byte $0F, $09, $19, $29

hello:
.byte 'H', 'E', 'L', 'L', 'O', ' ', 'W', 'O', 'R', 'L', 'D', $00
; ------------------------------------------------------------------------------
