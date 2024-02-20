; Pokémon Stadium 2's "GB Tower" emulator contains a very peculiar boot ROM.
; It can be found at offset 0x015995f0 in the US release

INCLUDE "hardware.inc/hardware.inc"


DEF COLOR_MASK equ $1F ; Each color spans 5 bits
DEF COLOR_MAX equ $1F
DEF COLOR_SIZE equ 2 ; Colors are encoded as 2-byte little-endian BGR555
DEF COLORS_PER_PALETTE equ 4
DEF PALETTE_SIZE equ COLOR_SIZE * COLORS_PER_PALETTE

DEF TILE_SIZE equ 16

DEF NB_OBJS equ 40 ; OAM contains 40 OBJs
DEF OBJ_SIZE equ 4 ; Each OAM OBJ is 4 bytes
DEF OAM_SIZE equ NB_OBJS * OBJ_SIZE

DEF WAVE_RAM_SIZE equ 16


; "GAME BOY" logo constants
DEF GB_LOGO_HEIGHT equ 3 ; The logo is 3 tile rows tall
DEF GB_LOGO_WIDTH equ 16 ; The logo is 12 tiles wide
DEF LOGO_BLOCK_WIDTH equ 3 ; Width in tiles of one color "block"
DEF NB_LOGO_PALETTES equs "((BootAnimationColorsEnd - BootAnimationColors) / COLOR_SIZE)"
DEF GB_LOGO_FIRST_TILE equs "LOW(vGameBoyLogoTiles / TILE_SIZE)"

; Old "Nintendo" logo constants
DEF OLD_LOGO_HEIGHT equ 2
DEF OLD_LOGO_WIDTH equ 12


MACRO gdma_params
    db HIGH(\1), LOW(\1)
    db HIGH(\2), LOW(\2)
    db \3 / 16
ENDM

MACRO rgb
    REPT _NARG / 3
        dw (\1 & $1F) | ((\2 & $1F) << 5) | ((\3 & $1F) << 10)
        SHIFT 3
    ENDR
ENDM


DEF rBANK equ $FF50 ; Boot ROM lockout reg


SECTION "First section", ROM0[$0000]

EntryPoint:
    ld sp, hStackBottom
    ld a, $80
    ldh [rBCPS], a
    ld c, LOW(rBCPD)
    xor a
    ld b, 8 * COLORS_PER_PALETTE * COLOR_SIZE
.clearBGPalettes
    ldh [c], a
    dec b
    jr nz, .clearBGPalettes
    jp Main


ClearLogoGDMA:
    ; Clear 9 rows starting at the one above the "GAME BOY" logo
    gdma_params wZeroBuffer, (vGameBoyLogoMap & -SCRN_VX_B) - SCRN_VX_B, SCRN_VX_B * 9
ClearLogoGDMAEnd:
ClearLogoTilesGDMA:
    gdma_params wZeroBuffer, vTiles, $40 * TILE_SIZE


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
RTileEnd:


Main:
    ldh [rLCDC], a
    ld a, %11111100
    ldh [rBGP], a
    call SetupSound
    call ClearVRAM
    ; Clear WRA2
    ld h, HIGH(wWorkRAM)
    call ClearUntilMemBoundary ; Basically until $E000
    ld hl, _OAMRAM
    ld c, OAM_SIZE
    xor a
.clearOAM
    ld [hli], a
    dec c
    jr nz, .clearOAM

    ld de, HeaderLogo
    ld hl, vLogoTiles
    ld c, h ; ld c, $80 = LOW(hLogoBuffer)
.processLogo
    ld a, [de]
    ldh [c], a
    inc c
    call DecompressFirstNibble
    call DecompressSecondNibble
    inc de
    ld a, e
    cp LOW(HeaderTitle)
    jr nz, .processLogo

    ; ld hl, vRTile
    ld de, RTile
    ld b, RTileEnd - RTile
.copyRTile
    ld a, [de]
    inc de
    ld [hli], a
    inc hl
    dec b
    jr nz, .copyRTile

    call SetupGameBoyLogo

    ld a, BANK(vGameBoyLogoAttrs)
    ldh [rVBK], a
    ld a, LCDCF_ON | LCDCF_BLK01 | LCDCF_BGON
    ldh [rLCDC], a
    ld hl, vGameBoyLogoMap - GB_LOGO_WIDTH
    ld b, $4E ; Counter that ticks upwards, controls SFX timing
    ld c, 150 ; For how many frames to run the animation
    call DoLogoAnimation
    xor a
    ldh [rVBK], a

    call PerformFadeout
    jr .nop
.nop
    call WaitVBlank
    ldh [rBANK], a
.lockup
    jr .lockup


ClearVRAM:
    ld hl, _VRAM
ClearUntilMemBoundary:
    xor a
.loop
    ld [hli], a
    bit 5, h
    jr z, .loop
    ret

Memcpy:
    ld a, [hli]
    ld [de], a
    inc de
    dec c
    jr nz, Memcpy
    ret


SetupSound:
    ld a, $80
    ldh [rNR52], a ; Write AUDENA_ON
    ldh [rNR11], a ; Write AUDLEN_DUTY_50
    ld a, $F3
    ldh [rNR12], a ; Write 15 << 4 | AUDENV_DOWN | 3
    ; Note that only channel 1 will be used!
    ldh [rNR51], a ; Route all channels to left speaker, channels 1 and 2 to right
    ; Set both speakers to max volume, and ignore VIN
    ld a, $77
    ldh [rNR50], a
    ld hl, _AUD3WAVERAM
    xor a
    ld c, WAVE_RAM_SIZE
.initWaveRAM
    ld [hli], a
    cpl
    dec c
    jr nz, .initWaveRAM
    ret


; Nibbles are stored "vertically", ie they store the following nibbles of a tile:
; 0 4  8 C  and so on
; 1 5  9 D
; 2 6  A E
; 3 7  B F
; Two nibbles are decoded per loop iteration, ie a quarter of a tile
; Note that due to the way the nibbles are stored, only one half of the logo
; can be decoded at once
DecodeLogoHalf:
    ld c, 0
.decodeTileQuarter
    ld a, [de]
    and $F0
    bit 1, c
    jr z, .decodingLeftHalf
    swap a
.decodingLeftHalf
    ld b, a
    inc hl
    ld a, [hl]
    or b
    ld [hli], a
    ld a, [de]
    and $0F
    bit 1, c
    jr nz, .decodingRightHalf
    swap a
.decodingRightHalf
    ld b, a
    inc hl
    ld a, [hl]
    or b
    ld [hli], a
    inc de
    bit 0, c
    jr z, .decodingTopHalf ; We will naturally slide into the second half
    push de
    ld de, -(TILE_SIZE / 2)
    bit 1, c
    jr z, .goToRightHalf ; Go back to the beginning of the tile
    ld de, TILE_SIZE / 2 ; Go to the next tile
.goToRightHalf
    add hl, de
    pop de
.decodingTopHalf
    inc c
    ld a, c
    cp (HeaderTitle - HeaderLogo) / 2
    jr nz, .decodeTileQuarter
    ret


WaitVBlank:
    push hl
    ld hl, rIF
    res 0, [hl]
.wait
    bit 0, [hl]
    jr z, .wait
    pop hl
    ret


SECTION "Header", ROM0[$0100] ; Yes, for some reason this boot ROM has a valid header and all

    nop
    jp EntryPoint ; Hard to tell if the target was explicitly $0000, or if it was just left blank

    ; Each tile is encoded using 2 (!) bytes
    ; The tiles are represented below
    ; Read nibbles block-row by block-row, block by block, row by row
    ; XX.. .XX. XX.. .... .... .... .... .... .... ...X X... ....
    ; XXX. .XX. XX.. .... ..XX .... .... .... .... ...X X... ....
    ; XXX. .XX. .... .... .XXX X... .... .... .... ...X X... ....
    ; XX.X .XX. XX.X X.XX ..XX ..XX XX.. XX.X X... XXXX X..X XXX.
    ;
    ; XX.X .XX. XX.X XX.X X.XX .XX. .XX. XXX. XX.X X..X X.XX ..XX
    ; XX.. XXX. XX.X X..X X.XX .XXX XXX. XX.. XX.X X..X X.XX ..XX
    ; XX.. XXX. XX.X X..X X.XX .XX. .... XX.. XX.X X..X X.XX ..XX
    ; XX.. .XX. XX.X X..X X.XX ..XX XXX. XX.. XX.. XXXX X..X XXX.
HeaderLogo:
    db $CE,$ED, $66,$66, $CC,$0D, $00,$0B, $03,$73, $00,$83, $00,$0C, $00,$0D, $00,$08, $11,$1F, $88,$89, $00,$0E
    db $DC,$CC, $6E,$E6, $DD,$DD, $D9,$99, $BB,$BB, $67,$63, $6E,$0E, $EC,$CC, $DD,$DC, $99,$9F, $BB,$B9, $33,$3E
HeaderTitle:
    ds 15, 0 ; Title
    db CART_COMPATIBLE_DMG_GBC
    dw 0 ; New licensee
    db 0 ; SGB flag: not SGB capable
    db 0 ; Cart type: ROM
    db 0 ; ROM size: 32 KiB
    db 0 ; SRAM size: 0
    db 0 ; Destination code: Japanese
    db 0 ; Old licensee (not SGB capable)
    db 0 ; Version number
    db $67 ; Header checksum
    dw 0 ; Global checksum (bad)


; @param hl Ptr to OBJ pal data
; @param b How many bytes of OBJ pal data to write
; @param hl+b+d Ptr to BG pal data
; @param e How many bytes of BG pal data to write
SetOBJAndBGPals:
    ld a, $80 ; Address 0, auto-increment
    ldh [rBCPS], a
    ldh [rOCPS], a
    ld c, LOW(rOCPD)
.writeOBJPalData
    ld a, [hli]
    ldh [c], a
    dec b
    jr nz, .writeOBJPalData
    ld c, d
    add hl, bc
    ld b, e
    ld c, LOW(rBCPD)
.writeBGPalData
    ld a, [hli]
    ldh [c], a
    dec b
    jr nz, .writeBGPalData
    ret

; Update BG palettes and a single byte of OBJ palettes because we have to
; Copies the whole wBGPalBuffer to color palette RAM
; @destroys AF
CommitBGPalettes:
    push bc
    push de
    push hl
    ; This function fairly obviously wants to only update BG palettes, but the
    ; callee requires updating at least 1 byte of OBJ palette
    ; This makes the first byte of OBJ palettes initialized (other bytes are
    ; not, which has them random on hardware)
    ld hl, wOBJPalBuffer
    ld b, 1
    ld d, (wBGPalBuffer - wOBJPalBuffer) - 1
    ld e, wBGPalBufferEnd - wBGPalBuffer
    call SetOBJAndBGPals
    pop hl
    pop de
    pop bc
    ret


; B counts upwards during the logo animation and controls the timing of SFX playback
; C counts how many iterations remain until the end of the animation,
; each iteration taking two frames (because)—the loop runs at 30 fps!
; Called with C = 68 and B = $4E
DoLogoAnimation:
    call WaitVBlank
    call CommitBGPalettes

    ; When 119 iterations remain (12 ones after the beginning),
    ; draw the Nintendo logo
    ld a, c
    cp 119
    jr nz, .dontWriteNintendoLogo
    push hl
    xor a ; ld a, BANK(vTilemap)
    ldh [rVBK], a
    ld hl, vNintendoLogoMap
    ld a, LOW(vNintendoLogoTiles / TILE_SIZE)
.writeNintendoLogoMap
    ld [hli], a
    inc a
    cp LOW(vNintendoLogoTilesEnd / TILE_SIZE)
    jr nz, .writeNintendoLogoMap
    ld a, BANK(vAttrMap)
    ldh [rVBK], a
    pop hl
.dontWriteNintendoLogo

    call WaitVBlank

    ; Animate the logo when there are less than 48 remaining iterations
    ld a, c
    sub 58
    jp nc, .dontAnimateLogo
    ld a, c
    cp 1
    jp z, .dontAnimateLogo
    ; Check if we're past the last tile (i.e. we finished animating)
    ld a, l
    cp LOW(vGameBoyLogoAttrs + GB_LOGO_WIDTH - 1)
    jr z, .dontWriteLogoAttrMap
    push bc
    ld b, GB_LOGO_HEIGHT ; Number of rows
.changePaletteRow
    ld c, 1 ; Start at palette 1, the first non-blank one
.changePaletteBlock
    ld d, LOGO_BLOCK_WIDTH
.changePaletteLoop
    ld a, [hl]
    and $F8
    or c
    ld [hli], a
    dec d
    jr nz, .changePaletteLoop
    inc c
    ld a, c
    cp NB_LOGO_PALETTES
    jr nz, .changePaletteBlock
    ; Add the remainder to advance to the next row
    ld de, SCRN_VX_B - LOGO_BLOCK_WIDTH * (NB_LOGO_PALETTES - 1)
    add hl, de
    dec b
    jr nz, .changePaletteRow
    ; Go back to first row, but move right one tile
    ld de, -(SCRN_VX_B * GB_LOGO_HEIGHT - 1)
    add hl, de
    pop bc
.dontWriteLogoAttrMap

    ; Check if should play SFX
    inc b
    ld a, b
    ld e, LOW($783)
    cp $4E + 20
    jr z, .playSFX
    ld e, LOW($7C1)
    cp $4E + 22
    jr nz, .dontPlaySFX
.playSFX
    ld a, e
    ldh [rNR13], a
    ld a, AUDHIGH_RESTART | 7
    ldh [rNR14], a
.dontPlaySFX
.dontAnimateLogo

    ; Prevent ticking the loop if the following counter is non-zero and
    ; doing so would terminate it
    ld a, [wPreventTerminationCounter]
    cp 0
    jr z, .stepAnimation
    dec a
    ld [wPreventTerminationCounter], a
    ld a, c
    cp 1
    jp z, DoLogoAnimation
.stepAnimation
    dec c
    jp nz, DoLogoAnimation
    ret


PerformFadeout:
    ld c, 38 ; Duration of the fadeout, in frames
.loop
    call .fadePalettes
    call WaitVBlank
    call WaitVBlank
    call CommitBGPalettes
    dec c
    jr nz, .loop
    call WaitVBlank
    ld a, BANK(vAttrMap)
    ldh [rVBK], a
    call .clearLogoArea
    ; ld hl, ClearLogoTilesGDMA
    call .clearLogoTiles
    xor a ; ld a, BANK(vTilemap)
    ldh [rVBK], a
    call .clearLogoArea
    ret

.clearLogoArea
    ld hl, ClearLogoGDMA
.clearLogoTiles
    ld de, rHDMA1
    ld c, ClearLogoGDMAEnd - ClearLogoGDMA ; Size of GDMA arguments
    call Memcpy
    ret

.fadePalettes
    push bc
    push de
    push hl
    ld hl, wBGPalBuffer
    ld c, (wBGPalBuffer - wOBJPalBuffer) / COLOR_SIZE ; Size of any palette buffer
.fadeColor
    ; Deal with red component
    ld a, [hl]
    and COLOR_MASK ; Extract red component
    cp COLOR_MAX
    jr z, .redCap
    inc a
.redCap
    ld d, a
    ; Deal with green component, which is split across the two bytes
    ld a, [hli]
    rlca
    rlca
    rlca
    and HIGH(LOW(COLOR_MASK << 5) << 3) ; Extract low bits of green component
    ld b, a
    ld a, [hld]
    rlca
    rlca
    rlca
    and HIGH(COLOR_MASK << 5) << 3 ; Extract high bits of green component
    or b
    cp COLOR_MAX
    jr z, .greenCap
    inc a
.greenCap
    ; Rotate the bits back into place
    rrca
    rrca
    rrca
    ld b, a
    and LOW(COLOR_MASK << 5) ; Extract low bits of green...
    or d ; ...mix with red...
    ld [hli], a ; ... and write back
    ld a, b
    and HIGH(COLOR_MASK << 5) ; Extract high bits of green...
    ld e, a ; ...and store for later
    ; Deal with blue component
    ld a, [hl]
    rrca
    rrca
    and COLOR_MASK
    cp COLOR_MAX
    jr z, .blueCap
    inc a
.blueCap
    rlca
    rlca
    or e
    ld [hli], a
    dec c
    jr nz, .fadeColor
    pop hl
    pop de
    pop bc
    ret


; Upscale a nibble (1x4 pixels) by 2 (2x8 pixels)
DecompressFirstNibble:
    ld b, a
DecompressSecondNibble:
    push de
    ld d, 4
.decompressBit
    ld e, b
    rl b
    rla
    rl e
    rla
    dec d
    jr nz, .decompressBit
    pop de
    ld [hli], a
    inc hl
    ld [hli], a
    inc hl
    ret

WriteLogoTilemap:
    ld a, LOW(vRTile / TILE_SIZE)
    ld [vBigNintendoLogoMap + OLD_LOGO_WIDTH], a

    ld hl, vBigNintendoLogoMap + SCRN_VX_B + OLD_LOGO_WIDTH - 1
.writeRow
    ld c, OLD_LOGO_WIDTH
.writeByte
    dec a
    jr z, .done ; ynawt `ret z`?
    ld [hld], a
    dec c
    jr nz, .writeByte
    ld l, LOW(vBigNintendoLogoMap + OLD_LOGO_WIDTH - 1)
    jr .writeRow
.done
    ret

SetupGameBoyLogo:
    ld a, BANK(vGameBoyLogoTiles)
    ldh [rVBK], a
    call ClearVRAM
    ld de, GameBoyLogoTiles
    ld hl, vGameBoyLogoTiles
    ld c, GameBoyLogoTilesEnd - GameBoyLogoTiles
.copyLogoRow
    ld a, [de]
    ld [hli], a
    inc hl
    ld [hli], a
    inc hl
    inc de
    dec c
    jr nz, .copyLogoRow

    ld de, HeaderLogo
    call DecodeLogoHalf
    ld bc, -$58 ; Go backwards 5 and a half tiles
    add hl, bc
    call DecodeLogoHalf
    ld bc, -8 ; Go backwards half a tile
    add hl, bc
    ld de, RTile
    ld c, 8
.copyRTile
    inc hl
    ld a, [de]
    ld [hli], a
    inc de
    dec c
    jr nz, .copyRTile

    ld hl, vGameBoyLogoAttrs
    ld b, 8
    ld a, 8
.writeAttrRow
    ld c, 16
.writeAttrByte
    ld [hli], a
    dec c
    jr nz, .writeAttrByte
    ld de, SCRN_VX_B - 16
    add hl, de
    dec b
    jr nz, .writeAttrRow

    xor a ; ld a, BANK(vGameBoyLogoMap)
    ldh [rVBK], a
    ld hl, vGameBoyLogoMap
    ld a, 8
.writeTilemapByte
    ld [hli], a
    inc a
    cp 8 + GB_LOGO_WIDTH * 1
    jr nz, .notFirstRow
    ld l, LOW(vGameBoyLogoMap + SCRN_VX_B)
.notFirstRow
    cp 8 + GB_LOGO_WIDTH * 2
    jr nz, .notSecondRow
    ld hl, vGameBoyLogoMap + SCRN_VX_B * 2
.notSecondRow
    cp 8 + 16 * 3
    jr nz, .writeTilemapByte

    ; This reads 8 colors from `BootAnimationColors`, which only contains 6
    ld hl, BootAnimationColors
    ld de, wBGPalBuffer
    ld b, (wBGPalBufferEnd - wBGPalBuffer) / 8
.initBGPalsLoop
    ; White
    ld a, $FF
    ld [de], a
    inc de
    ld [de], a
    inc de
    ; Some color
    ld c, 2
    call Memcpy
    ; Black
    ld a, 0
    ld [de], a
    inc de
    ld [de], a
    inc de
    ; Black as well
    inc de
    inc de
    dec b
    jr nz, .initBGPalsLoop
    call CommitBGPalettes
    ret


GameBoyLogoTiles:
    ; Each row is 1bpp (different colors are achieved through attrmap modifications), halving the size
    ; And then the logo is upscaled vertically (doubled), halving yet again

    ; TODO: use a more explicit format instead of a raw dump
    db $01, $0F, $3F, $7E
    db $FF, $FF, $C0, $00
    db $C0, $F0, $F1, $03
    db $7C, $FC, $FE, $FE
    db $03, $07, $07, $0F
    db $E0, $E0, $F0, $F0
    db $1E, $3E, $7E, $FE
    db $0F, $0F, $1F, $1F
    db $FF, $FF, $00, $00
    db $01, $01, $01, $03
    db $FF, $FF, $E1, $E0
    db $C0, $F0, $F9, $FB
    db $1F, $7F, $F8, $E0
    db $F3, $FD, $3E, $1E
    db $E0, $F0, $F9, $7F
    db $3E, $7C, $F8, $E0
    db $F8, $F0, $F0, $F8
    db $00, $00, $7F, $7F
    db $07, $0F, $9F, $BF
    db $9E, $1F, $FF, $FF
    db $0F, $1E, $3E, $3C
    db $F1, $FB, $7F, $7F
    db $FE, $DE, $DF, $9F
    db $1F, $3F, $3E, $3C
    db $F8, $F8, $00, $00
    db $03, $03, $07, $07
    db $FF, $FF, $C1, $C0
    db $F3, $E7, $F7, $F3
    db $C0, $C0, $C0, $C0
    db $1F, $1F, $1E, $3E
    db $3F, $1F, $3E, $3E
    db $80, $00, $00, $00
    db $7C, $1F, $07, $00
    db $0F, $FF, $FE, $00
    db $7C, $F8, $F0, $00
    db $1F, $0F, $0F, $00
    db $7C, $F8, $F8, $00
    db $3F, $3E, $1C, $00
    db $0F, $0F, $0F, $00
    db $7C, $FF, $FF, $00
    db $00, $F8, $F8, $00
    db $07, $0F, $0F, $00
    db $81, $FF, $FF, $00
    db $F3, $E1, $80, $00
    db $E0, $FF, $7F, $00
    db $FC, $F0, $C0, $00
    db $3E, $7C, $7C, $00
    db $00, $00, $00, $00
GameBoyLogoTilesEnd:


BootAnimationColors:
    rgb $1F, $1F, $1F ; White
    rgb $00, $00, $1F ; Blue
    rgb $00, $1F, $00 ; Green
    rgb $1F, $00, $1F ; Purple
    rgb $1F, $00, $00 ; Red
    rgb $1F, $1F, $00 ; Yellow
BootAnimationColorsEnd:

    ds 4, 0 ; Padding

    ; Total: $3F0 bytes. Not $400, because..?



SECTION "Video RAM", VRAM[$8000],BANK[0]

vBlankTile:
    ds $10
vLogoTiles:
    ds $10 * (HeaderTitle - HeaderLogo) / 2
vRTile:
    ds $10

SECTION "Video RAM bank 1", VRAM[$8000],BANK[1]
vTiles:

    ds $80

vGameBoyLogoTiles:
    ds (GameBoyLogoTilesEnd - GameBoyLogoTiles) * 4
vNintendoLogoTiles:
    ds 6 * 16
vSecondRTile:
    ds 16
vNintendoLogoTilesEnd:

;; Definition of VRAM layout

MACRO vram_block
    ; Y rows + X columns
    SECTION "\1 tilemap", VRAM[$9800 + (\3) * SCRN_VX_B + (\2)],BANK[0]
    \1Map:
    SECTION "\1 attrmap", VRAM[$9800 + (\3) * SCRN_VX_B + (\2)],BANK[1]
    \1Attrs:
ENDM

; Actual definition...

SECTION "Tilemap", VRAM[_SCRN0],BANK[0]
vTileMap:
SECTION "Attrmap", VRAM[_SCRN0],BANK[1]
vAttrMap:

    vram_block vGameBoyLogo, 2, 6, GB_LOGO_WIDTH, GB_LOGO_HEIGHT
    vram_block vNintendoLogo, 7, 13, vNintendoLogoTilesEnd - vNintendoLogoTiles + 1, 1 ; +1 for the "®"

    vram_block vBigNintendoLogo, 4, 8, OLD_LOGO_WIDTH, OLD_LOGO_HEIGHT


SECTION "Work RAM", WRAMX[$D000],BANK[2]
wWorkRAM:

; Commented-out variable names are ones present in the stock CGB boot ROM, that wind up unused here

;wTitleChecksum:
    ds 1

    ds 1

wPreventTerminationCounter:
    ds 1

;wHeldButtons:
    ds 1
;wPressedButtons: ; This is never read, only written to
    ds 1

;wPaletteOverrideIndex:
    ds 1

    ; TODO: some bytes around WhichPalTriplet and D00B are used in specific circumstances, investigate and label them
;wWhichPalTripletCopy:
    ds 1
;wOldWhichPalTriplet:
    ds 1
;wWhichPalTriplet:
    ds 1
;wPalShufflingFlagsCopy: ; This is only written to by the CGB0 boot ROM. Debugging purposes?
    ds 1
;wOldPalShufflingFlags: ; This saves the previous value of wPalShufflingFlags, maybe for Debugging purposes?
    ds 1
; Explanation at WriteShuffledPalTriplets
;wPalShufflingFlags:
    ds 1

    ds $2F4

wZeroBuffer:
    ds $400

    ds $100

wOBJPalBuffer:
    ds 8 * 8
wOBJPalBufferEnd:

wBGPalBuffer:
    ds 8 * 8
wBGPalBufferEnd:

    ds $80

;wPalIndexBuffer:
    ds 90
;wPalIndexBufferEnd:
    ds 6
;wPalIndexBufferRealEnd:

    ds $A0

;wPalBuffer:
    ds 96 * 8


SECTION "High RAM", HRAM[$FF80]

    ds $7E
hStackBottom:
