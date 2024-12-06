
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
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Sprite object attribute memory (256B)
; ------------------------------------------------------------------------------
.segment "OAM"
; ------------------------------------------------------------------------------
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Remainder of RAM area for variable storage
; ------------------------------------------------------------------------------
.segment "BSS"
; ------------------------------------------------------------------------------
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Main application entry point; called on startup and reset
; ------------------------------------------------------------------------------
.segment "CODE"
; ------------------------------------------------------------------------------
.proc reset
    sei      ; Set interrupt disable
    cld      ; Clear decimal mode
    ldx #$FF
    txs      ; Transfer x (255) to the stack

wait_vblank1:
    bit $2002        ; Test PPU status bits in memory with accumulator
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

wait_vblank2:
    bit $2002        ; Test PPU status bits in memory with accumulator
    bpl wait_vblank2 ; Loop while the accumulator is positive

    jmp main ; Start the main loop
.endproc
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; NMI routine, called every vertical blank
; ------------------------------------------------------------------------------
.segment "CODE"
; ------------------------------------------------------------------------------
.proc nmi
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
mainloop:
    jmp mainloop
.endproc
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Read-only data
; ------------------------------------------------------------------------------
.segment "RODATA"
; ------------------------------------------------------------------------------
; ------------------------------------------------------------------------------
