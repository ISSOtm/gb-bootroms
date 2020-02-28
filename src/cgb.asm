INCLUDE "hardware.inc"
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

gdma_params: MACRO
    db HIGH(\1), LOW(\1)
    db HIGH(\2), LOW(\2)
    db \3 / 16
ENDM

rgb: MACRO
    REPT _NARG / 3
        dw (\1 & $1F) | ((\2 & $1F) << 5) | ((\3 & $1F) << 10)
        SHIFT
        SHIFT
        SHIFT
    ENDR
ENDM


SECTION "First section", ROM0[$000]

EntryPoint:
    ld sp, hStackBottom
    ld a, BANK("Work RAM")
    jp Setup


ClearLogoGDMA:
    gdma_params $D300, vMainTilemap + SCRN_VX_B * 5, 9 * SCRN_VX_B
ClearLogoTilesGDMA:
    gdma_params $D300, $8000, $400


OverrideColors:
    rgb $1e,$18,$14, $10,$06,$00
    rgb $1f,$10,$10, $1c,$00,$00
    rgb $14,$10,$0a, $0d,$0a,$06
    rgb $0c,$14,$1f, $00,$00,$1f
    rgb $11,$11,$1b, $0a,$0a,$11
    rgb $12,$12,$12, $00,$00,$00
    rgb $1f,$1f,$14, $1f,$00,$1f
    rgb $1f,$1f,$00, $1f,$00,$00
    rgb $1f,$1f,$07, $07,$05,$00
    rgb $0f,$1f,$06, $1f,$00,$00
    rgb $0f,$1f,$06, $00,$00,$1f
    rgb $00,$00,$00, $1f,$1f,$00


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
LogoTopHalf:
    db $CE,$ED, $66,$66, $CC,$0D, $00,$0B, $03,$73, $00,$83, $00,$0C, $00,$0D, $00,$08, $11,$1F, $88,$89, $00,$0E
LogoBottomHalf: ; The boot ROM only checks for the top half, but the full logo is present anyways
    db $DC,$CC, $6E,$E6, $DD,$DD, $D9,$99, $BB,$BB, $67,$63, $6E,$0E, $EC,$CC, $DD,$DC, $99,$9F, $BB,$B9, $33,$3E


RTile:
    gfx ..XXXX..
    gfx .X....X.
    gfx X.XXX..X
    gfx X.X..X.X
    gfx X.XXX..X
    gfx X.X..X.X
    gfx .X....X.
    gfx ..XXXX..


; Titles matching these checksums need the Nintendo logo tilemap in VRAM when they boot up
LogoTilemapChecksums:
    db $58, $43


Setup:
    ldh [rSVBK], a
    ld a, $FC
    ldh [rBGP], a
    call SetupSound
    call ClearVRAM
    ; Clear WRA2
    ld h, $D0
    call ClearUntilMemBoundary
    ld hl, _OAMRAM
    ld c, $A0
    xor a
.clearOAM
    ld [hli], a
    dec c
    jr nz, .clearOAM

    ld de, HeaderLogo
    ld hl, vLogoTiles
    ld c, h ; ld c, LOW(hLogoBuffer)
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
    ld b, 8
.copyRTile
    ld a, [de]
    inc de
    ld [hli], a
    inc hl
    dec b
    jr nz, .copyRTile

    call SetupGameBoyLogo

    ld a, 1
    ldh [rVBK], a
    ld a, LCDCF_ON | LCDCF_BG8000 |LCDCF_BGON
    ldh [rLCDC], a
    ld hl, vMainAttrMap + SCRN_VX_B * 5 + 18
    ld b, $4E ; TODO:
    ld c, $44 ; TODO:
    call DoLogoAnimation
    xor a
    ldh [rVBK], a

    ld c, LOW(hLogoBuffer)
    ld hl, LogoTopHalf
    ld b, (HeaderTitle - HeaderLogo) / 2
.checkLogo
    ldh a, [c]
    inc c
    cp [hl]
.logoFailure
    jr nz, .logoFailure
    inc hl
    dec b
    jr nz, .checkLogo

    ld hl, HeaderTitle
    ld b, $19
    ld a, b
.computeChecksum
    add a, [hl]
    inc l
    dec b
    jr nz, .computeChecksum
    add a, [hl]
.checksumFailure
    jr nz, .checksumFailure

    call PerformFadeout
    jr .done

    nop
IF !DEF(agb)
    nop
ENDC

.done
    call SetupCompatibility
    xor a
    ldh [rSVBK], a
; We basically know for sure that the AGB boot ROM simply inserts an extra `inc b`
; What we don't know is where
IF DEF(agb)
    inc b
ENDC
    ld a, $11
    ldh [$FF50], a


SECTION "Second section", ROM0[$200]

ClearVRAM:
    ld hl, $8000
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

WaitVBlank:
    push hl
    ld hl, rIF
    res 0, [hl]
.wait
    bit 0, [hl]
    jr z, .wait
    pop hl
    ret


PollJoypad:
    ld de, rP1
    ld hl, wHeldButtons
    ld c, $0F
    ld a, $30
    ld [de], a
    ld a, $20
    ld [de], a
    ld a, [de]
    cpl
    and c
    swap a
    ld b, a
    ld a, $10
    ld [de], a
    ld a, [de]
    cpl
    and c
    or b
    ld c, a ; Held buttons
    ld a, [hl]
    xor c
    and $F0
    ld b, a
    ld a, [hli]
    xor c
    and c
    or b
    ld [hld], a
    ld b, a
    ld a, c
    ld [hl], a
    ld a, $30
    ld [de], a
    ret


; @param hl Ptr to OBJ pal data
; @param b How many bytes of OBJ pal data to write
; @param hl+b+c Ptr to BG pal data
; @param e How many bytes of BG pal data to write
SetOBJAndBGPals:
    ld a, $80
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

CommitPalettes:
    push bc
    push de
    push hl
    ld hl, wOBJPalBuffer
    ld b, 1
    ld d, (wBGPalBuffer - wOBJPalBuffer) - 1
    ld e, 8 * 8
    call SetOBJAndBGPals
    pop hl
    pop de
    pop bc
    ret


SetupSound:
    ld a, $80
    ldh [rNR52], a
    ldh [rNR11], a
    ld a, $F3
    ldh [rNR12], a
    ldh [rNR51], a
    ld a, $77
    ldh [rNR50], a
IF !DEF(cgb0)
    ld hl, _AUD3WAVERAM
    xor a
    ld c, 16
.initWaveRAM
    ld [hli], a
    cpl
    dec c
    jr nz, .initWaveRAM
ENDC
    ret


DoLogoAnimation:
    call WaitVBlank
    call CommitPalettes
    ld a, c
    cp 56
    jr nz, .dontWriteNintendoLogo
    push hl
    xor a
    ldh [rVBK], a
    ld hl, vMainTilemap + SCRN_VX_B * 13 + 7
    ld a, LOW(vNintendoLogoTiles / 16)
.writeNintendoLogoMap
    ld [hli], a
    inc a
    cp LOW(vNintendoLogoTilesEnd / 16)
    jr nz, .writeNintendoLogoMap
    ld a, 1
    ldh [rVBK], a
    pop hl
.dontWriteNintendoLogo
    push bc
    push hl
    ld hl, HeaderCGBCompat
    bit 7, [hl]
    call z, PickDMGPalette
    pop hl
    pop bc

    call WaitVBlank
    ld a, c
    sub 48
    jp nc, .dontPlaySFX
    ld a, c
    cp 1
    jp z, .dontPlaySFX
    ld a, l
    cp LOW(vMainAttrMap + SCRN_VX_B * 6 + 17)
    jr z, .dontWriteLogoAttrMap
    push bc
    ld b, 3
.changePaletteRow
    ld c, 1 ; New palette number
.changePaletteBlock
    ld d, 3 ; Width of a block
.changePaletteLoop
    ld a, [hl]
    and $F8
    or c
    ld [hli], a
    dec d
    jr nz, .changePaletteLoop
    inc c
    ld a, c
    cp 6
    jr nz, .changePaletteBlock
    ld de, SCRN_VX_B - 3 * 5
    add hl, de
    dec b
    jr nz, .changePaletteRow
    ld de, -$5F
    add hl, de
    pop bc
.dontWriteLogoAttrMap
    inc b
    ld a, b
    ld e, $83
    cp $62
    jr z, .playSFX
    ld e, $C1
    cp $64
    jr nz, .dontPlaySFX
.playSFX
    ld a, e
    ldh [rNR13], a
    ld a, $87
    ldh [rNR14], a
.dontPlaySFX
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
    ld c, $26
.loop
    call FadePalettes
    call WaitVBlank
    call CommitPalettes
    dec c
    jr nz, .loop
    call WaitVBlank
    ld a, 1
    ldh [rVBK], a
    call .clearLogo
    ; ld hl, ClearLogoTilesGDMA
    call .clearLogoTiles
    xor a
    ldh [rVBK], a
    call .clearLogo
    ret

.clearLogo
    ld hl, ClearLogoGDMA
.clearLogoTiles
    ld de, rHDMA1
    ld c, 5
    call Memcpy
    ret

FadePalettes:
    push bc
    push de
    push hl
    ld hl, wBGPalBuffer
    ld c, (wBGPalBuffer - wOBJPalBuffer) / 2 ; Size of any palette buffer
.fadeColor
    ld a, [hl]
    and $1F ; Extract red component
    cp $1F
    jr z, .redCap
    inc a
.redCap
    ld d, a
    ld a, [hli]
    rlca
    rlca
    rlca
    and 7
    ld b, a
    ld a, [hld]
    rlca
    rlca
    rlca
    and $18
    or b
    cp $1F
    jr z, .greenCap
    inc a
.greenCap
    rrca
    rrca
    rrca
    ld b, a
    and $E0
    or d
    ld [hli], a
    ld a, b
    and 3
    ld e, a
    ld a, [hl]
    rrca
    rrca
    and $1F
    cp $1F
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
    ld de, -8
    bit 1, c
    jr z, .goToRightHalf ; Go back to the beginning of the tile
    ld de, 8 ; Go to the next tile
.goToRightHalf
    add hl, de
    pop de
.decodingTopHalf
    inc c
    ld a, c
    cp (HeaderTitle - HeaderLogo) / 2
    jr nz, .decodeTileQuarter
    ret


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
    ld a, $19
    ld [vMainTilemap + SCRN_VX_B * 8 + 16], a ; 8 rows down, 16 across
    ld hl, vMainTilemap + SCRN_VX_B * 9 + 15 ; 9 rows down, 15 across
.writeRow
    ld c, 12
.writeByte
    dec a
    jr z, .done ; ynawt `ret z`?
    ld [hld], a
    dec c
    jr nz, .writeByte
    ld l, LOW(vMainTilemap + SCRN_VX_B * 8 + 15)
    jr .writeRow
.done
    ret

SetupGameBoyLogo:
    ld a, 1
    ldh [rVBK], a
    call ClearVRAM
    ld de, GameBoyLogoTiles
    ld hl, vGameBoyLogoTiles
IF DEF(cgb0)
    ld b, $30
.copyLogoTile
    ld c, 4
ELSE
    ld c, GameBoyLogoTilesEnd - GameBoyLogoTiles
ENDC
.copyLogoRow
    ld a, [de]
    ld [hli], a
    inc hl
    ld [hli], a
    inc hl
    inc de
    dec c
    jr nz, .copyLogoRow
IF DEF(cgb0)
    dec b
    jr nz, .copyLogoTile
ENDC

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

    ld hl, vMainAttrMap + SCRN_VX_B * 6 + 2
    ld b, 8
    ld a, 8
.writeAttrRow
    ld c, 16
.writeAttrByte
    ld [hli], a
    dec c
    jr nz, .writeAttrByte
    ld de, 16
    add hl, de
    dec b
    jr nz, .writeAttrRow

    xor a
    ldh [rVBK], a
    ld hl, vMainTilemap + SCRN_VX_B * 6 + 2
    ld a, 8
.writeTilemapByte
    ld [hli], a
    inc a
    cp 8 + 16 * 1
    jr nz, .notFirstRow
    ld l, LOW(vMainTilemap + SCRN_VX_B * 7 + 2)
.notFirstRow
    cp 8 + 16 * 2
    jr nz, .notSecondRow
    ld hl, vMainTilemap + SCRN_VX_B * 8 + 2
.notSecondRow
    cp 8 + 16 * 3
    jr nz, .writeTilemapByte

    ld hl, BootAnimationColors
    ld de, wBGPalBuffer
    ld b, 8
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
    call CommitPalettes

    ld hl, HeaderOldLicensee
    ld a, [hl]
    cp $33
    jr nz, .usingOldLicensee
    ld l, LOW(HeaderNewLicensee)
    ld e, "0"
    ld a, [hli]
    cp e
    jr nz, .useIndex00
    inc e ; ld e, "1"
    jr .checkMadeByNintendo
.usingOldLicensee
    ld l, LOW(HeaderOldLicensee)
    ld e, 1
.checkMadeByNintendo
    ld a, [hli]
    cp e
    jr nz, .useIndex00
    ld l, LOW(HeaderTitle)
    ld bc, $0010
.checksumTitle
    ld a, [hli]
    add a, b
    ld b, a
    dec c
    jr nz, .checksumTitle
    ld [wTitleChecksum], a

    ld hl, TitleChecksums
    ld c, 0
.seekTitleChecksum
    ld a, [hli]
    cp b
    jr z, .foundTitleChecksum
    inc c
    ld a, c
    cp TitleChecksumsEnd - TitleChecksums
    jr nz, .seekTitleChecksum
    jr .useIndex00
.foundTitleChecksum
    ld a, c
    sub $41
    jr c, .gotIndex
    ld hl, TitleFourthLetters
    ld d, 0
    ld e, a
    add hl, de
.seekFourthLetter
    ld a, [HeaderTitle+3]
    ld d, a
    ld a, [hl]
    cp d
    jr z, .gotIndex
    ld de, 14
    add hl, de
    ld a, c
    add a, e
    ld c, a
    sub TitleFourthLettersEnd - TitleFourthLetters + $41
    jr c, .seekFourthLetter
.useIndex00
    ld c, 0
.gotIndex
    ld hl, PaletteIndexesAndFlags
    ld b, 0
    add hl, bc
    ld a, [hl]
    and $1F
IF DEF(cgb0)
    ld [wD006], a
ENDC
    ld [wD008], a
    ld a, [hl]
    and $E0
    rlca
    rlca
    rlca
IF DEF(cgb0)
    ld [wD009], a
ENDC
    ld [wD00B], a
    call Func_04E9
    ret

; TODO: label this properly (includes figuring out what the hell this does)
Func_04E9:
    ld de, PaletteIndexes
    ld hl, wPalIndexBuffer
    ld a, [wD00B]
    ld b, a
    ld c, (wPalIndexBufferEnd - wPalIndexBuffer) / 3
.get3Indexes
    bit 0, b
    jr nz, .bit0Set
    inc de
    inc de
.bit0Set
    ld a, [de]
    ld [hli], a
    jr nz, .cancelBit0Set
    dec de
    dec de
.cancelBit0Set
    bit 1, b
    jr nz, .bit1Set
    inc de
    inc de
.bit1Set
    ld a, [de]
    ld [hli], a
    inc de
    inc de
    jr nz, .cancelBit1Set
    dec de
    dec de
.cancelBit1Set
    bit 2, b
    jr z, .bit2Set
    dec de
    dec hl
    ld a, [de]
    ld [hli], a
    inc de
.bit2Set
    ld a, [de]
    ld [hli], a
    inc de
    dec c
    jr nz, .get3Indexes

    ld hl, wPalIndexBuffer
    ld de, wPalBuffer
    call GetPalettes
    ret


ApplyPaletteOverride:
    ld hl, OverrideColors
    ld a, [wPaletteOverrideIndex]
    rlca
    rlca
    ld b, 0
    ld c, a
    add hl, bc
    ld de, wBGPalBuffer ; Palette X color 0
    ld b, 8
.copyPalette
    push hl
    ld c, 2
    call Memcpy
REPT 8 - 2
    inc de
ENDR
    pop hl
    dec b
    jr nz, .copyPalette
    ld de, wBGPalBuffer+2 ; Palette 0 color 1
    ld c, 2
    call Memcpy
    ld de, wBGPalBuffer+8+2 ; Palette 1 color 1
    ld c, 2
    call Memcpy
    dec hl
    dec hl
    ld de, wBGPalBuffer+4 ; Palette 0 color 2
    ld c, 2
    call Memcpy
    ret


GetPalettes:
    ld c, 96
.copyPalette
    ld a, [hli]
    push hl
    push bc
    ld hl, Palettes
    ld b, 0
    ld c, a
    add hl, bc
    ld c, 8
    call Memcpy
    pop bc
    pop hl
    dec c
    jr nz, .copyPalette
    ret


Func_057B:
    ld a, [wD008]
    ld de, 8*3
    inc a
.loop
    dec a
    jr z, .done
    add hl, de
    jr nz, .loop
.done
    ret


PickDMGPalette:
    call PollJoypad
    ld a, b
    and a, $FF
    jr z, .jumpToDone
    ld hl, JoypadCombos
    ld b, 0
.seekButtonCombo
    ld a, [hli]
    cp c
    jr z, .buttonComboFound
    inc b
    ld a, b
    cp JoypadCombosEnd - JoypadCombos
    jr nz, .seekButtonCombo
.jumpToDone
    jr .done

.buttonComboFound
    ld a, b
    ld [wPaletteOverrideIndex], a
    ld a, 30
    ld [wPreventTerminationCounter], a
    ld de, JoypadCombosEnd - JoypadCombos - 1
    add hl, de
    ld d, [hl]
    ld a, d
    and $1F
    ld e, a
    ld hl, wD008
    ld a, [hld]
    ld [hli], a
    ld a, e
    ld [hl], a
    ld a, d
    and $E0
    rlca
    rlca
    rlca
    ld e, a
    ld hl, wD00B
    ld a, [hld]
    ld [hli], a
    ld a, e
    ld [hl], a
    call Func_04E9
    call ApplyPaletteOverride
.done
    ret


SetupCompatibility:
    call WaitVBlank
    ld a, [HeaderCGBCompat]
    bit 7, a
    jr z, .dmgMode
    ; See below for what this is
    ldh [$FF4C], a
    jr .done

    ; Credit to @mattcurrie for finding what those registers are!
.dmgMode
    ; "CPU mode register" according to Fig. 11 in this patent:
    ; https://patents.google.com/patent/US6322447B1/en?oq=US6322447bi
    ld a, $04
    ldh [$FF4C], a
    ; "OBJ priority mode designating register" in the same patent
    ld a, 1
    ldh [$FF6C], a
    ld hl, wPalBuffer
    call Func_057B
    ld b, 16
    ld d, 0
    ld e, 8
    call SetOBJAndBGPals
    ld hl, LogoTilemapChecksums
    ld a, [wTitleChecksum]
    ld b, a
    ld c, 2
.tryWriteLogoTilemap
    ld a, [hli]
    cp b
    call z, WriteLogoTilemap
    dec c
    jr nz, .tryWriteLogoTilemap
.done
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


TitleChecksums:
    db $00, $88, $16, $36, $D1, $DB, $F2, $3C, $8C, $92, $3D, $5C, $58, $C9, $3E, $70
    db $1D, $59, $69, $19, $35, $A8, $14, $AA, $75, $95, $99, $34, $6F, $15, $FF, $97
    db $4B, $90, $17, $10, $39, $F7, $F6, $A2, $49, $4E, $43, $68, $E0, $8B, $F0, $CE
    db $0C, $29, $E8, $B7, $86, $9A, $52, $01, $9D, $71, $9C, $BD, $5D, $6D, $67, $3F
    db $6B
    ; These checksums are also discriminated based on the 4th title letter
    db $B3, $46, $28, $A5, $C6, $D3, $27, $61, $18, $66, $6A, $BF, $0D, $F4
TitleChecksumsEnd:

TitleFourthLetters:
  ; $4? 123456789ABCDE
    db "BEFAARBEKEK R-"
    db "URAR INAILICE "
    db "R"
TitleFourthLettersEnd:


PaletteIndexesAndFlags:
    ; TODO: improve this dump
    db $7C, $08, $12, $A3, $A2, $07, $87, $4B, $20, $12, $65, $A8, $16, $A9, $86, $B1, $68, $A0, $87, $66, $12, $A1, $30, $3C, $12, $85, $12, $64, $1B, $07, $06, $6F, $6E, $6E, $AE, $AF, $6F, $B2, $AF, $B2, $A8, $AB, $6F, $AF, $86, $AE, $A2, $A2, $12, $AF, $13, $12, $A1, $6E, $AF, $AF, $AD, $06, $4C, $6E, $AF, $AF, $12, $7C, $AC, $A8, $6A, $6E, $13, $A0, $2D, $A8, $2B, $AC, $64, $AC, $6D, $87, $BC, $60, $B4, $13, $72, $7C, $B5, $AE, $AE, $7C, $7C, $65, $A2, $6C, $64, $85

PaletteIndexes:
    ; TODO: improve this
    db $80, $B0, $40, $88, $20, $68, $DE, $00, $70, $DE, $20, $78, $20, $20, $38, $20, $B0, $90, $20, $B0, $A0, $E0, $B0, $C0, $98, $B6, $48, $80, $E0, $50, $1E, $1E, $58, $20, $B8, $E0, $88, $B0, $10, $20, $00, $10, $20, $E0, $18, $E0, $18, $00, $18, $E0, $20, $A8, $E0, $20, $18, $E0, $00, $20, $18, $D8, $C8, $18, $E0, $00, $E0, $40, $28, $28, $28, $18, $E0, $60, $20, $18, $E0, $00, $00, $08, $E0, $18, $30, $D0, $D0, $D0, $20, $E0, $E8

Palettes:
    ; TODO: since those are colors, format using `rgb` macro instead
    db $FF, $7F, $BF, $32, $D0, $00, $00, $00
    db $9F, $63, $79, $42, $B0, $15, $CB, $04
    db $FF, $7F, $31, $6E, $4A, $45, $00, $00
    db $FF, $7F, $EF, $1B, $00, $02, $00, $00
    db $FF, $7F, $1F, $42, $F2, $1C, $00, $00
    db $FF, $7F, $94, $52, $4A, $29, $00, $00
    db $FF, $7F, $FF, $03, $2F, $01, $00, $00
    db $FF, $7F, $EF, $03, $D6, $01, $00, $00
    db $FF, $7F, $B5, $42, $C8, $3D, $00, $00
    db $74, $7E, $FF, $03, $80, $01, $00, $00
    db $FF, $67, $AC, $77, $13, $1A, $6B, $2D
    db $D6, $7E, $FF, $4B, $75, $21, $00, $00
    db $FF, $53, $5F, $4A, $52, $7E, $00, $00
    db $FF, $4F, $D2, $7E, $4C, $3A, $E0, $1C
    db $ED, $03, $FF, $7F, $5F, $25, $00, $00
    db $6A, $03, $1F, $02, $FF, $03, $FF, $7F
    db $FF, $7F, $DF, $01, $12, $01, $00, $00
    db $1F, $23, $5F, $03, $F2, $00, $09, $00
    db $FF, $7F, $EA, $03, $1F, $01, $00, $00
    db $9F, $29, $1A, $00, $0C, $00, $00, $00
    db $FF, $7F, $7F, $02, $1F, $00, $00, $00
    db $FF, $7F, $E0, $03, $06, $02, $20, $01
    db $FF, $7F, $EB, $7E, $1F, $00, $00, $7C
    db $FF, $7F, $FF, $3F, $00, $7E, $1F, $00
    db $FF, $7F, $FF, $03, $1F, $00, $00, $00
    db $FF, $03, $1F, $00, $0C, $00, $00, $00
    db $FF, $7F, $3F, $03, $93, $01, $00, $00
    db $00, $00, $00, $42, $7F, $03, $FF, $7F
    db $FF, $7F, $8C, $7E, $00, $7C, $00, $00
    db $FF, $7F, $EF, $1B, $80, $61, $00, $00


BootAnimationColors:
    rgb $1F, $1F, $1F
    rgb $00, $00, $1F
    rgb $00, $1F, $00
    rgb $1F, $00, $1F
    rgb $1F, $00, $00
    rgb $1F, $1F, $00


JoypadCombos:
    db PADF_UP
    db PADF_UP    | PADF_A
    db PADF_UP    | PADF_B
    db PADF_LEFT
    db PADF_LEFT  | PADF_A
    db PADF_LEFT  | PADF_B
    db PADF_DOWN
    db PADF_DOWN  | PADF_A
    db PADF_DOWN  | PADF_B
    db PADF_RIGHT
    db PADF_RIGHT | PADF_A
    db PADF_RIGHT | PADF_B
JoypadCombosEnd:


; Unused data, Liji says those are joypad combo-induced palette indexes?
    db $12, $B0, $79, $B8, $AD, $16, $17, $07, $BA, $05, $7C, $13, $00, $00, $00, $00



SECTION "Video RAM", VRAM[$8000],BANK[0]

vBlankTile:
    ds $10
vLogoTiles:
    ds $10 * (HeaderTitle - HeaderLogo) / 2
vRTile:
    ds $10

SECTION "Video RAM bank 1", VRAM[$8000],BANK[1]

    ds $80

vGameBoyLogoTiles:
    ds (GameBoyLogoTilesEnd - GameBoyLogoTiles) * 4
vNintendoLogoTiles:
    ds 6 * 16
vSecondRTile:
    ds 16
vNintendoLogoTilesEnd:

SECTION "Tilemap", VRAM[$9800],BANK[0]

vMainTilemap:
    ds SCRN_VX_B * SCRN_VY_B

SECTION "Attr map", VRAM[$9800],BANK[1]

vMainAttrMap:
    ds SCRN_VX_B * SCRN_VY_B


SECTION "Work RAM", WRAMX[$D000],BANK[2]

wTitleChecksum:
    ds 1

    ds 1

wPreventTerminationCounter:
    ds 1

wHeldButtons:
    ds 1
wPressedButtons:
    ds 1

wPaletteOverrideIndex:
    ds 1

    ; TODO: some bytes around D008 and D00B are used in specific circumstances, investigate and label them
wD006:
    ds 1
    ds 1
wD008:
    ds 1
wD009:
    ds 1
    ds 1
wD00B:
    ds 1

    ds $7F4

wOBJPalBuffer:
    ds 8 * 8

wBGPalBuffer:
    ds 8 * 8

    ds $80

wPalIndexBuffer:
    ds 90
wPalIndexBufferEnd:

    ds $A6

wPalBuffer:
    ds 96 * 8


SECTION "High RAM", HRAM[$FF80]

hLogoBuffer:
    ; ds ?
    ds $7E

hStackBottom:
