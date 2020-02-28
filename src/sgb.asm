INCLUDE "hardware.inc/hardware.inc"
INCLUDE "header.inc"

; Macro to allow defining gfx in a clearer format
; Basically RGBASM's "dw `(...)" but for 1bpp
gfx: MACRO
POSITION = 0
VALUE = 0
BITS = 0
    REPT STRLEN("\1")
POSITION = POSITION + 1
VALUE = VALUE << 1
BITS = BITS + 1
        IF !STRCMP("X", STRSUB("\1", POSITION, 1))
            ; If pixel is turned on, set LSB
VALUE = VALUE | 1
        ELIF STRCMP(".", STRSUB("\1", POSITION, 1))
            ; If char is padding, skip it
VALUE = VALUE >> 1
BITS = BITS - 1
        ENDC

        IF BITS == 8
            db VALUE
VALUE = 0
BITS = 0
        ENDC
    ENDR
ENDM


SECTION "Boot ROM", ROM0[$000]

EntryPoint:
    ld sp, hStackBottom
    ld a, $30
    ldh [rP1], a

    xor a
    ld hl, $9FFF
.clearVRAM
    ld [hld], a
    bit 7, h
    jr nz, .clearVRAM

    ld hl, rNR52
    ld c, LOW(rNR11) ; CH1 length
    ; Enable APU
    ; This sets (roughly) all audio registers to 0
    ld a, $80
    ld [hld], a
    ; hl = rNR51
    ; Set CH1 duty cycle to 25%
    ldh [c], a
    inc c ; ld c, LOW(rNR11) ; CH1 envelope
    ld a, $F3 ; Initial volume 15, 3 decreasing sweep
    ldh [c], a
    ; Route all channels to left speaker, CH2 and CH1 to right speaker
    ld [hld], a
    ; hl = rNR50
    ; Set volume on both speakers to 7, disable VIN on both speakers
    ld a, $77
    ld [hl], a

    ld a, $FC
    ldh [rBGP], a


    ld hl, BufferEnd - 1
    ld c, 8
    xor a
.clearBuffer
    ld [hld], a
    dec c
    jr nz, .clearBuffer

    ld de, HeaderGlobalChecksum+1
    ld a, $FB ; Packet ID
    ld c, $06
.copyHeader
    push af
    ld b, 0
.computeChecksum
    ld a, [de]
    dec de
    ld [hld], a
    add a, b
    ld b, a
    dec c
    jr nz, .computeChecksum
    ld [hld], a
    pop af
    ld [hld], a
    ld c, 14
    sub 2
    cp $EF
    jr nz, .copyHeader

    ld de, HeaderLogo
    ld hl, vLogoTiles
.decompressHeader
    ld a, [de]
    call DecompressFirstNibble
    call DecompressSecondNibble
    inc de
    ld a, e
    cp LOW(HeaderTitle)
    jr nz, .decompressHeader

    ld de, RTile
    ld b, 8
.copyRTile
    ld a, [de]
    inc de
    ld [hli], a
    inc hl
    dec b
    jr nz, .copyRTile
    ld a, $19
    ld [vMainTilemap + SCRN_VX_B * 8 + 16], a ; 8 rows down, 16 across

    ld hl, vMainTilemap + SCRN_VX_B * 9 + 15
.writeTilemapRow
    ld c, 12
.writeTilemapByte
    dec a
    jr z, SendData
    ld [hld], a
    dec c
    jr nz, .writeTilemapByte
    ld l, LOW(vMainTilemap + SCRN_VX_B * 8 + 15)
    jr .writeTilemapRow

SendData:
    ld a, LCDCF_ON | LCDCF_BG8000 | LCDCF_BGON
    ldh [rLCDC], a

    ld hl, Buffer
    ld c, LOW(rP1)
.sendPacket
    ld a, 0
    ldh [c], a
    ld a, $30
    ldh [c], a
    ld b, 16
.sendByte
    ld e, 8
    ld a, [hli]
    ld d, a
.sendBit
    bit 0, d
    ld a, $10
    jr nz, .gotBit
    ld a, $20 ; Both code paths aren't equal CPU time, if you had used `add a, a` we wouldn't have so many headaches NINTENDOOOOO >:(
.gotBit
    ldh [c], a
    ld a, $30
    ldh [c], a
    rr d
    dec e
    jr nz, .sendBit
    dec b
    jr nz, .sendByte
    ld a, $20
    ldh [c], a
    ld a, $30
    ldh [c], a
    call Wait4Frames
    ld a, l
    cp LOW(BufferEnd)
    jr nz, .sendPacket

    ld c, LOW(rNR13)
    ld a, $C1
    ldh [c], a
    inc c
    ld a, 7
    ldh [c], a
    jr Done

Wait4Frames:
    ld d, 4
.waitVBlank
    ldh a, [rLY]
    cp SCRN_Y
    jr nz, .waitVBlank
    ld e, 0
.wait
    dec e
    jr nz, .wait
    dec d
    jr nz, .waitVBlank
    ret


DecompressFirstNibble:
    ld c, a
DecompressSecondNibble:
    ld b, 4
.decompressBit
    push bc
    rl c
    rla
    pop bc
    rl c
    rla
    dec b
    jr nz, .decompressBit
    ld [hli], a
    inc hl
    ld [hli], a
    inc hl
    ret

RTile:
    gfx ..XXXX..
    gfx .X....X.
    gfx X.XXX..X
    gfx X.X..X.X
    gfx X.XXX..X
    gfx X.X..X.X
    gfx .X....X.
    gfx ..XXXX..


REPT 14
    db 0
ENDR


Done:
IF DEF(sgb2)
    ld a, $FF
ELSE
    ld a, 1
ENDC
    ldh [$FF50], a



SECTION "VRAM tiles", VRAM[$8000],BANK[0]

vBlankTile:
    ds $10
vLogoTiles:
    ds $10 * (HeaderTitle - HeaderLogo) / 2
vRTile:
    ds $10

SECTION "VRAM tilemap", VRAM[$9800],BANK[0]

vMainTilemap:
    ds SCRN_VX_B * SCRN_VY_B


SECTION "WRAM", WRAM0[$C000]

Buffer:
    ds $60
BufferEnd:



SECTION "HRAM", HRAM[$FFEE]

    ds $10
hStackBottom:
