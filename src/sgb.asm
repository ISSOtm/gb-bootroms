INCLUDE "hardware.inc/hardware.inc"
INCLUDE "constants.inc"
INCLUDE "header.inc"


SECTION "Boot ROM", ROM0[$000]

EntryPoint:
    ld sp, hStackBottom

    ld a, $30
    ldh [rP1], a

    xor a
    ld hl, _VRAM + SIZEOF(VRAM) - 1
.clearVRAM
    ld [hld], a
    bit 7, h
    jr nz, .clearVRAM

    ld hl, rNR52
    ld c, LOW(rNR11) ; CH1 length
    ; Enable APU
    ; This sets (roughly) all audio registers to 0
    ld a, AUDENA_ON
    ld [hld], a
    ASSERT rNR52 - 1 == rNR51
    ; Set CH1 duty cycle to 25%
    ASSERT AUDENA_ON == AUDLEN_DUTY_50
    ldh [c], a
    inc c
    ASSERT LOW(rNR11) + 1 == LOW(rNR12)
    ld a, (15 << 4) | AUDENV_DOWN | 3 ; Initial volume 15, decreasing sweep 3
    ldh [c], a
    ; Route all channels to left speaker, CH2 and CH1 to right speaker
    ; Note that only channel 1 will be used!
    ld [hld], a
    ASSERT rNR51 - 1 == rNR50
    ; Set volume on both speakers to max, disable VIN on both speakers
    ld a, $77
    ld [hl], a

    ld a, %11_11_11_00
    ldh [rBGP], a


    ld hl, wBufferEnd - 1
    ld c, 8
    xor a
.clearBuffer
    ld [hld], a
    dec c
    jr nz, .clearBuffer

    ld de, HeaderGlobalChecksum + 1
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
    ld a, LCDCF_ON | LCDCF_BLK01 | LCDCF_BGON
    ldh [rLCDC], a

    ld hl, wBuffer
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
    cp LOW(wBufferEnd)
    jr nz, .sendPacket

    ; Set frequency to $7C1
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
    PUSHO
    OPT b.X
    db %..XXXX..
    db %.X....X.
    db %X.XXX..X
    db %X.X..X.X
    db %X.XXX..X
    db %X.X..X.X
    db %.X....X.
    db %..XXXX..
    POPO


    ds 14, 0 ; Unused space


Done:
IF DEF(sgb2)
    ld a, $FF
ELSE
    ld a, 1
ENDC
    ldh [$FF50], a



SECTION "VRAM tiles", VRAM[_VRAM], BANK[0]

vBlankTile:
    ds TILE_SIZE
vLogoTiles:
    ds (HeaderTitle - HeaderLogo) * TILE_SIZE / 2
vRTile:
    ds TILE_SIZE

SECTION "VRAM tilemap", VRAM[_SCRN0], BANK[0]

vMainTilemap:
    ds SCRN_VX_B * SCRN_VY_B


SECTION "WRAM", WRAM0[_RAM]

wBuffer:
    ds $60
wBufferEnd:



SECTION "HRAM", HRAM[$FFEE]

    ds $10
hStackBottom:
