INCLUDE "hardware.inc/hardware.inc"
INCLUDE "constants.inc"
INCLUDE "header.inc"


SECTION "Boot ROM 1", ROM0[$0000]

EntryPoint:
    ld sp, hStackBottom
    ld a, BANK("Work RAM")
    jp Setup


ClearLogoGDMA:
    ; Clear 9 rows starting at the one above the "GAME BOY" logo
    gdma_params wZeroBuffer, (vGameBoyLogoMap & -SCRN_VX_B) - SCRN_VX_B, SCRN_VX_B * 9
.end
ClearLogoTilesGDMA:
    gdma_params wZeroBuffer, vTiles, $40 * TILE_SIZE


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


; Whitespace is not stripped after line continuations until RGBDS v0.6.0, so rows are not indented
LogoTopHalf:
    logo_row_gfx_nybbles \
XX.., .XX., XX.., ...., ...., ...., ...., ...., ...., ...X, X..., ...., \
XXX., .XX., XX.., ...., ..XX, ...., ...., ...., ...., ...X, X..., ...., \
XXX., .XX., ...., ...., .XXX, X..., ...., ...., ...., ...X, X..., ...., \
XX.X, .XX., XX.X, X.XX, ..XX, ..XX, XX.., XX.X, X..., XXXX, X..X, XXX.
LogoBottomHalf:
    logo_row_gfx_nybbles \
XX.X, .XX., XX.X, XX.X, X.XX, .XX., .XX., XXX., XX.X, X..X, X.XX, ..XX, \
XX.., XXX., XX.X, X..X, X.XX, .XXX, XXX., XX.., XX.X, X..X, X.XX, ..XX, \
XX.., XXX., XX.X, X..X, X.XX, .XX., ...., XX.., XX.X, X..X, X.XX, ..XX, \
XX.., .XX., XX.X, X..X, X.XX, ..XX, XXX., XX.., XX.., XXXX, X..X, XXX.


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
.end


; Titles matching these checksums need the Nintendo logo tilemap in VRAM when they boot up
LogoTilemapChecksums:
    db $58, $43
.end


Setup:
    ldh [rSVBK], a
    ld a, %11_11_11_00
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
    ASSERT HIGH(vLogoTiles) == LOW(hLogoBuffer)
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
    ld b, RTile.end - RTile
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
    ld c, 68 ; For how many frame to run the animation
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
    ld b, $19 ; Checksum starting value
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
IF !(DEF(agb0) || DEF(agb))
    jr .done
ELSE
    nop
ENDC
    nop
    nop

.done
    call SetupCompatibility
    xor a
    ldh [rSVBK], a
IF DEF(agb0) || DEF(agb)
    inc b ; Increment b for GBA identification
ENDC
    ld a, $11
    ldh [rBANK], a


SECTION "Boot ROM 2", ROM0[$0200]

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
    ld a, P1F_GET_NONE
    ld [de], a
    ld a, P1F_GET_DPAD
    ld [de], a
    ld a, [de]
    cpl
    and c
    swap a
    ld b, a
    ld a, P1F_GET_BTN
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
    ld [hld], a ; Write to wPressedButtons
    ld b, a
    ld a, c
    ld [hl], a
    ld a, P1F_GET_NONE
    ld [de], a
    ret


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
    ld e, wBGPalBuffer.end - wBGPalBuffer
    call SetOBJAndBGPals
    pop hl
    pop de
    pop bc
    ret


SetupSound:
    ; Enable APU
    ld a, AUDENA_ON
    ldh [rNR52], a
    ; Set CH1 duty cycle to 50%
    ASSERT AUDENA_ON == AUDLEN_DUTY_50
    ldh [rNR11], a
    ld a, (15 << 4) | AUDENV_DOWN | 3 ; Initial volume 15, decreasing sweep 3
    ldh [rNR12], a
    ; Note that only channel 1 will be used!
    ldh [rNR51], a ; Route all channels to left speaker, channels 1 and 2 to right
    ; Set volume on both speakers to max, disable VIN on both speakers
    ld a, $77
    ldh [rNR50], a
    ; CGB0 boot ROM didn't init wave RAM!
IF !DEF(cgb0)
    ld hl, _AUD3WAVERAM
    xor a
    ld c, WAVE_RAM_SIZE
.initWaveRAM
    ld [hli], a
    cpl
    dec c
    jr nz, .initWaveRAM
ENDC
    ret


; B counts upwards during the logo animation and controls the timing of SFX playback
; C counts how many iterations remain until the end of the animation,
; each iteration taking two frames (because)—the loop runs at 30 fps!
; Called with C = 68 and B = $4E
DoLogoAnimation:
    call WaitVBlank
    call CommitBGPalettes

    ; When 56 iterations remain (12 ones after the beginning),
    ; draw the Nintendo logo
    ld a, c
    cp 56
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

    push bc
    push hl
    ld hl, HeaderCGBCompat
    bit 7, [hl]
    call z, PickDMGPalette
    pop hl
    pop bc

    call WaitVBlank

    ; Animate the logo when there are less than 48 remaining iterations
    ld a, c
    sub 48
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
    and ~OAMF_PALMASK
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
    ASSERT HIGH($783) == HIGH($7C1)
    ld a, AUDHIGH_RESTART | HIGH($783)
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
    ld c, ClearLogoGDMA.end - ClearLogoGDMA ; Size of GDMA arguments
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
IF DEF(cgb0)
    ld b, (GameBoyLogoTiles.end - GameBoyLogoTiles) / 4
.copyLogoTile
    ld c, 4
ELSE
    ld c, GameBoyLogoTiles.end - GameBoyLogoTiles
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

; cgbE and later revisions of the GBA fixed the logo TOCTTOU
IF !DEF(cgbE) && !DEF(agb)
    ld de, HeaderLogo
ELSE
    ld de, hLogoBuffer
ENDC
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
    ld b, (wBGPalBuffer.end - wBGPalBuffer) / 8
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


    ld hl, HeaderOldLicensee
    ld a, [hl]
    cp $33
    jr nz, .usingOldLicensee
    ; New licensee field is 2 bytes, check the first one now
    ld l, LOW(HeaderNewLicensee)
    ld e, "0"
    ld a, [hli]
    cp e
    jr nz, .useDefaultIndex
    inc e ; ld e, "1"
    ; ...and re-use the code below to finish the check
    jr .checkMadeByNintendo
.usingOldLicensee
    ld l, LOW(HeaderOldLicensee)
    ld e, 1
.checkMadeByNintendo
    ; Nintendo has licensee code 01, which is either hex 01 or ASCII "01"
    ; for old and new licensee field, respectively.
    ; Previous code loaded the correct byte in E
    ld a, [hli]
    cp e
    jr nz, .useDefaultIndex

    ld l, LOW(HeaderTitle)
    ld bc, 0 << 8 | 16 ; Sum in B, remaining length in C
.checksumTitle
    ld a, [hli]
    add a, b
    ld b, a
    dec c
    jr nz, .checksumTitle
    ld [wTitleChecksum], a

    ; Try to find the checksum in the database
    ld hl, TitleChecksums
    ld c, 0
.seekTitleChecksum
    ld a, [hli]
    cp b
    jr z, .foundTitleChecksum
    inc c
    ld a, c
    cp TitleChecksums.end - TitleChecksums
    jr nz, .seekTitleChecksum
    jr .useDefaultIndex

.foundTitleChecksum
    ; Some checksums need to be further disambiguated based on the 4th title byte
    ; ("Letter" is a bit of a misnomer because it can be a space or a hyphen)
    ld a, c
    sub TitleChecksums.ambiguous - TitleChecksums
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
    ; Advance search by one row
    ld de, TitleFourthLetters.row - TitleFourthLetters
    add hl, de
    ; Similarly advance index
    ld a, c
    add a, e
    ld c, a
    ; Compare index against max one
    sub (TitleFourthLetters.end - TitleFourthLetters) \ ; Max index
      + (TitleChecksums.ambiguous - TitleChecksums) ; (not forgetting base index)
    ; If there's some indexes left to check, keep going
    jr c, .seekFourthLetter
    ; If not, use default index

.useDefaultIndex
    ld c, 0
.gotIndex
    ld hl, PalTripletIDsAndFlags
    ld b, 0
    add hl, bc
    ld a, [hl]
    and COLOR_MASK
IF DEF(cgb0)
    ld [wWhichPalTripletCopy], a
ENDC
    ld [wWhichPalTriplet], a
    ld a, [hl]
    and $E0
    rlca
    rlca
    rlca
IF DEF(cgb0)
    ld [wPalShufflingFlagsCopy], a
ENDC
    ld [wPalShufflingFlags], a
    ; Nintendo: "damn, we don't have enough room in the boot ROM to clear wave RAM!"
    ; Also Nintendo:
    call WriteShuffledPalTriplets ; The label is literally right below,
    ret ; these two instructions are dead code!

; This shuffles **all** the predefined palette index triplets (`PaletteOffsets`) depending on the
; shuffling bits read from the entry in `PalTripletIDsAndFlags`.
; For each triplet, the behavior is as follows, depending on the bits in `wPalShufflingFlags`
; (which, mind you, correspond to the upper 3 bits read from `PalTripletIDsAndFlags`):
; - The third offset in the triplet (BGP's) will always be unchanged, so BGP is basically a baseline
; - By default (if no bit is set), the third offset will be copied to all 3, so both OBJ palettes
;   will match BGP
; - If bit 0 is set, the first offset (OBP0's) will instead be unchanged.
; - If bit 1 is set, the second offset (OBP1's) will instead use the first "source" offset.
;   (Thus, if bit 0 was also set, both OBJ palettes will be identical; if it wasn't, OBP0 will
;   match BGP, but OBP1 won't.)
; - If bit 2 is set, the second offset (OBP1's) will instead be unchanged; this overrides bit 1 if
;   it was set.
; Honestly, the way it's carried out is almost obfuscated, lol
WriteShuffledPalTriplets:
    ld de, PaletteOffsets
    ld hl, wPalOfsBuffer
    ld a, [wPalShufflingFlags]
    ld b, a
    ld c, (wPalOfsBuffer.end - wPalOfsBuffer) / 3 ; = 90 / 3 = 30
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
    jr z, .bit2Reset
    dec de
    dec hl
    ld a, [de]
    ld [hli], a
    inc de
.bit2Reset
    ld a, [de]
    ld [hli], a
    inc de
    dec c
    jr nz, .get3Indexes

    ld hl, wPalOfsBuffer
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

; @param hl Pointer to palette indexes
; @param de Pointer to buffer where to write palettes
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


; Get the selected palette triplet in the given buffer
; @param hl Base of the palette array
; @return hl Pointer to the triplet whose index is in `wWhichPalTriplet`
AddPalTripletOffset:
    ld a, [wWhichPalTriplet]
    ld de, PALETTE_SIZE * 3
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
    cp JoypadCombos.end - JoypadCombos
    jr nz, .seekButtonCombo
.jumpToDone
    jr .done

.buttonComboFound
    ld a, b
    ld [wPaletteOverrideIndex], a
    ld a, 30
    ld [wPreventTerminationCounter], a
    ld de, JoypadCombosTripletIDsAndFlags - JoypadCombos - 1
    add hl, de
    ld d, [hl]
    ld a, d
    and COLOR_MASK
    ld e, a
    ld hl, wWhichPalTriplet
    ld a, [hld]
    ld [hli], a
    ld a, e
    ld [hl], a
    ld a, d
    and ~COLOR_MASK
    rlca
    rlca
    rlca
    ld e, a
    ld hl, wPalShufflingFlags
    ; Save wPalShufflingFlags to wOldPalShufflingFlags
    ld a, [hld]
    ld [hli], a
    ld a, e
    ld [hl], a
    call WriteShuffledPalTriplets
    call ApplyPaletteOverride
.done
    ret


SetupCompatibility:
    call WaitVBlank
    ld a, [HeaderCGBCompat]
    bit 7, a
    jr z, .dmgMode
    ldh [rKEY0], a
    jr .done

.dmgMode
    ld a, KEY0F_DMG
    ldh [rKEY0], a
    ld a, OPRI_COORD
    ldh [rOPRI], a

    ld hl, wPalBuffer
    call AddPalTripletOffset
    ld b, PALETTE_SIZE * 2 ; Write 8 OBJ colors (OBP0 and OBP1)
    ld d, 0 ; 0 bytes between the OBJ and BG data
    ld e, PALETTE_SIZE ; Write 4 BG colors (BGP)
    call SetOBJAndBGPals

    ld hl, LogoTilemapChecksums
    ld a, [wTitleChecksum]
    ld b, a
    ld c, LogoTilemapChecksums.end - LogoTilemapChecksums
.tryWriteLogoTilemap
    ld a, [hli]
    cp b
    ; BUG: If it writes the logo tilemap, this loop only manages to exit thanks to dumb luck.
    ;
    ; As you can see, this loop exits when C reaches 0.
    ; Since the loop itself does not `push bc`, it follows that `WriteLogoTilemap` must preserve C,
    ; right..?
    ; Well, if you go check, you will notice that it doesn't! In fact, it also trashes HL, which
    ; also alters the loop's body!! (See `ld` above.) At least B is preserved, however.
    ; Due to the way it's written, the function returns with HL = $990F and C = $0C.
    ; This means that, after writing the logo tilemap, this loop will iterate 1 + 11 times
    ; (with the first iteration being only partial, since it begins after the function returns)
    ; and will thus read 11 bytes starting at $990F... but this is in the middle of tilemap!
    ; Three factors here allow the loop misbehaving:
    ; - This code is executed early enough in VBlank that under the worst theoretical conditions,
    ;   the function still exits during scanline $97, leaving enough time for the loop to read
    ;   11 bytes from VRAM safely before exiting.
    ; - The function ends up reading from the tilemap, which was largely cleared by the GDMA at the
    ;   end of `PerformFadeout`, so the area around the new logo tilemap is zeroes.
    ; - Thus, the bytes that will be read by the `ld a, [hli]` above end up being $xx, $19, and
    ;   then 9 zero bytes. Fortunately, this does not match either of the "logo tilemap checksums"
    ;   (remember, the title checksum has been preserved in B, and it must be one of those
    ;   checksums for the function to be executed at all), so the loop will exit.
    ; Make this four factors if you also want to count that the corrupted loop count is small
    ; enough that the loop always exits before the end of VBlank; if it were 0, this could have been
    ; different.
    ; All in all, it's highly unlikely that this behavior is intentional, especially since a lot of
    ; functions unnecessarily push and pop registers (for example, `PerformFadeout.fadePalettes`);
    ; it rather seems that whoever wrote this ROM did not save registers in this one place where it
    ; did matter, but was lucky enough to get away with it unscathed.
    call z, WriteLogoTilemap
    dec c
    jr nz, .tryWriteLogoTilemap
.done
    ret


GameBoyLogoTiles:
; Whitespace is not stripped after line continuations until RGBDS v0.6.0, so rows are not indented
    logo_row_gfx_bytes \
.......X, XXXXXXXX, XX......, .XXXXX.., ......XX, XXX....., ...XXXX., ....XXXX, XXXXXXXX, .......X, XXXXXXXX, XX......, ...XXXXX, XXXX..XX, XXX....., ..XXXXX., \
....XXXX, XXXXXXXX, XXXX...., XXXXXX.., .....XXX, XXX....., ..XXXXX., ....XXXX, XXXXXXXX, .......X, XXXXXXXX, XXXX...., .XXXXXXX, XXXXXX.X, XXXX...., .XXXXX.., \
..XXXXXX, XX......, XXXX...X, XXXXXXX., .....XXX, XXXX...., .XXXXXX., ...XXXXX, ........, .......X, XXX....X, XXXXX..X, XXXXX..., ..XXXXX., XXXXX..X, XXXXX..., \
.XXXXXX., ........, ......XX, XXXXXXX., ....XXXX, XXXX...., XXXXXXX., ...XXXXX, ........, ......XX, XXX....., XXXXX.XX, XXX....., ...XXXX., .XXXXXXX, XXX.....
    logo_row_gfx_bytes \
XXXXX..., ........, .....XXX, X..XXXX., ....XXXX, XXXX...X, XXXXXXX., ...XXXXX, XXXXX..., ......XX, XXXXXXXX, XXXX..XX, XX......, ...XXXXX, ..XXXXXX, X......., \
XXXX...., ........, ....XXXX, ...XXXXX, ...XXXX., XXXXX.XX, XX.XXXX., ..XXXXXX, XXXXX..., ......XX, XXXXXXXX, XXX..XXX, XX......, ...XXXXX, ...XXXXX, ........, \
XXXX...., .XXXXXXX, X..XXXXX, XXXXXXXX, ..XXXXX., .XXXXXXX, XX.XXXXX, ..XXXXX., ........, .....XXX, XX.....X, XXXX.XXX, XX......, ...XXXX., ..XXXXX., ........, \
XXXXX..., .XXXXXXX, X.XXXXXX, XXXXXXXX, ..XXXX.., .XXXXXXX, X..XXXXX, ..XXXX.., ........, .....XXX, XX......, XXXX..XX, XX......, ..XXXXX., ..XXXXX., ........
    logo_row_gfx_bytes \
.XXXXX.., ....XXXX, .XXXXX.., ...XXXXX, .XXXXX.., ..XXXXXX, ....XXXX, .XXXXX.., ........, .....XXX, X......X, XXXX..XX, XXX....., XXXXXX.., ..XXXXX., ........, \
...XXXXX, XXXXXXXX, XXXXX..., ....XXXX, XXXXX..., ..XXXXX., ....XXXX, XXXXXXXX, XXXXX..., ....XXXX, XXXXXXXX, XXX....X, XXXXXXXX, XXXX...., .XXXXX.., ........, \
.....XXX, XXXXXXX., XXXX...., ....XXXX, XXXXX..., ...XXX.., ....XXXX, XXXXXXXX, XXXXX..., ....XXXX, XXXXXXXX, X......., .XXXXXXX, XX......, .XXXXX.., ........, \
........, ........, ........, ........, ........, ........, ........, ........, ........, ........, ........, ........, ........, ........, ........, ........
.end


; The position of the cartridge's title checksum in this table is used as the index
; into `PalTripletIDsAndFlags`, except for the "ambiguous" checksums.
TitleChecksums:
    ; Each line is 8 entries wide
    db $00, $88, $16, $36, $D1, $DB, $F2, $3C
    db $8C, $92, $3D, $5C, $58, $C9, $3E, $70
    db $1D, $59, $69, $19, $35, $A8, $14, $AA
    db $75, $95, $99, $34, $6F, $15, $FF, $97
    db $4B, $90, $17, $10, $39, $F7, $F6, $A2
    db $49, $4E, $43, $68, $E0, $8B, $F0, $CE
    db $0C, $29, $E8, $B7, $86, $9A, $52, $01
    db $9D, $71, $9C, $BD, $5D, $6D, $67, $3F
    db $6B
.ambiguous
    ; These 14 checksums are also discriminated based on the 4th title letter, see table right below
    db $B3, $46, $28, $A5, $C6, $D3, $27, $61, $18, $66, $6A, $BF, $0D, $F4
.end

; How to read this: letters for index $41 are the leftmost ones, letters for $4E are the rightmost
; ones, etc. The table must then be read vertically, top to bottom.
; For example, the letters for $41 are B, U, and R, in that order.
TitleFourthLetters:
; $4x:   123456789ABCDE
     db "BEFAARBEKEK R-"
.row db "URAR INAILICE "
     db "R"
.end


; For each of these, the lower 5 bits indicate which row of `PaletteOffsets` to use.
; The upper 3 bits indicate how the palette shall be "shuffled".
; See `WriteShuffledPalTriplets` for an explanation of how they work.
MACRO idx_flgs
    REPT _NARG / 2
        db (\1) << 5 | (\2)
        SHIFT 2
    ENDR
ENDM
PalTripletIDsAndFlags:
    ; Each line is 8 entries wide
    idx_flgs %011,28, %000, 8, %000,18, %101, 3, %101, 2, %000, 7, %100, 7, %010,11
    idx_flgs %001, 0, %000,18, %011, 5, %101, 8, %000,22, %101, 9, %100, 6, %101,17
    idx_flgs %011, 8, %101, 0, %100, 7, %011, 6, %000,18, %101, 1, %001,16, %001,28
    idx_flgs %000,18, %100, 5, %000,18, %011, 4, %000,27, %000, 7, %000, 6, %011,15
    idx_flgs %011,14, %011,14, %101,14, %101,15, %011,15, %101,18, %101,15, %101,18
    idx_flgs %101, 8, %101,11, %011,15, %101,15, %100, 6, %101,14, %101, 2, %101, 2
    idx_flgs %000,18, %101,15, %000,19, %000,18, %101, 1, %011,14, %101,15, %101,15
    idx_flgs %101,13, %000, 6, %010,12, %011,14, %101,15, %101,15, %000,18, %011,28
    idx_flgs %101,12
    ; Those correspond to the "ambiguous" checksums, laid out like the letter disambiguation table
    idx_flgs %101, 8, %011,10, %011,14, %000,19, %101, 0, %001,13, %101, 8, %001,11, %101,12, %011, 4, %101,12, %011,13, %100, 7, %101,28
    idx_flgs %011, 0, %101,20, %000,19, %011,18, %011,28, %101,21, %101,14, %101,14, %011,28, %011,28, %011, 5, %101, 2, %011,12, %011, 4
    idx_flgs %100, 5

; These are byte offsets (read: "already multiplied by 8 = sizeof(Palette)") into `Palettes`.
; Well, except when Nintendo got clever, and used offsets that straddles entries to save space!
; The table entries should be read as `P*8+O`, with `P` being the palette ID (i.e. the line within
; `Palettes`), and `O` the offset within the palette (pointed out by a comment whenever relevant),
; omitted when there is none.
; In a given triplet:
; - BGP will always be the third entry.
; - OBP0 can be either the first or last entry.
; - OBP1 can be any entry.
PaletteOffsets:
    db 16*8,   22*8,    8*8
    db 17*8,    4*8,   13*8
    db 27*8+6,  0*8,   14*8
    db 27*8+6,  4*8,   15*8
    db  4*8,    4*8,    7*8

    db  4*8,   22*8,   18*8
    db  4*8,   22*8,   20*8
    db 28*8,   22*8,   24*8
    db 19*8,   22*8+6,  9*8
    db 16*8,   28*8,   10*8

    db  3*8+6,  3*8+6, 11*8
    db  4*8,   23*8,   28*8
    db 17*8,   22*8,    2*8
    db  4*8,    0*8,    2*8
    db  4*8,   28*8,    3*8

    db 28*8,    3*8,    0*8
    db  3*8,   28*8,    4*8
    db 21*8,   28*8,    4*8
    db  3*8,   28*8,    0*8
    db  4*8,    3*8,   27*8

    db 25*8,    3*8,   28*8
    db  0*8,   28*8,    8*8
    db  5*8,    5*8,    5*8
    db  3*8,   28*8,   12*8
    db  4*8,    3*8,   28*8

    db  0*8,    0*8,    1*8
    db 28*8,    3*8,    6*8
    db 26*8,   26*8,   26*8
    db  4*8,   28*8,   29*8

Palettes:
    rgb $1F,$1F,$1F, $1F,$15,$0C, $10,$06,$00, $00,$00,$00
    rgb $1F,$1C,$18, $19,$13,$10, $10,$0D,$05, $0B,$06,$01
    rgb $1F,$1F,$1F, $11,$11,$1B, $0A,$0A,$11, $00,$00,$00
    rgb $1F,$1F,$1F, $0F,$1F,$06, $00,$10,$00, $00,$00,$00 ; "3 plus 6" begins at this black
    rgb $1F,$1F,$1F, $1F,$10,$10, $12,$07,$07, $00,$00,$00

    rgb $1F,$1F,$1F, $14,$14,$14, $0A,$0A,$0A, $00,$00,$00
    rgb $1F,$1F,$1F, $1F,$1F,$00, $0F,$09,$00, $00,$00,$00
    rgb $1F,$1F,$1F, $0F,$1F,$00, $16,$0E,$00, $00,$00,$00
    rgb $1F,$1F,$1F, $15,$15,$10, $08,$0E,$0F, $00,$00,$00
    rgb $14,$13,$1F, $1F,$1F,$00, $00,$0C,$00, $00,$00,$00

    rgb $1F,$1F,$19, $0C,$1D,$1D, $13,$10,$06, $0B,$0B,$0B
    rgb $16,$16,$1F, $1F,$1F,$12, $15,$0B,$08, $00,$00,$00
    rgb $1F,$1F,$14, $1F,$12,$12, $12,$12,$1F, $00,$00,$00
    rgb $1F,$1F,$13, $12,$16,$1F, $0C,$12,$0E, $00,$07,$07
    rgb $0D,$1F,$00, $1F,$1F,$1F, $1F,$0A,$09, $00,$00,$00

    rgb $0A,$1B,$00, $1F,$10,$00, $1F,$1F,$00, $1F,$1F,$1F
    rgb $1F,$1F,$1F, $1F,$0E,$00, $12,$08,$00, $00,$00,$00
    rgb $1F,$18,$08, $1F,$1A,$00, $12,$07,$00, $09,$00,$00
    rgb $1F,$1F,$1F, $0A,$1F,$00, $1F,$08,$00, $00,$00,$00
    rgb $1F,$0C,$0A, $1A,$00,$00, $0C,$00,$00, $00,$00,$00

    rgb $1F,$1F,$1F, $1F,$13,$00, $1F,$00,$00, $00,$00,$00
    rgb $1F,$1F,$1F, $00,$1F,$00, $06,$10,$00, $00,$09,$00
    rgb $1F,$1F,$1F, $0B,$17,$1F, $1F,$00,$00, $00,$00,$1F ; "22 plus 6" begins at this blue
    rgb $1F,$1F,$1F, $1F,$1F,$0F, $00,$10,$1F, $1F,$00,$00
    rgb $1F,$1F,$1F, $1F,$1F,$00, $1F,$00,$00, $00,$00,$00

    rgb $1F,$1F,$00, $1F,$00,$00, $0C,$00,$00, $00,$00,$00
    rgb $1F,$1F,$1F, $1F,$19,$00, $13,$0C,$00, $00,$00,$00
    rgb $00,$00,$00, $00,$10,$10, $1F,$1B,$00, $1F,$1F,$1F ; "27 plus 6" begins at this white
    rgb $1F,$1F,$1F, $0C,$14,$1F, $00,$00,$1F, $00,$00,$00
    rgb $1F,$1F,$1F, $0F,$1F,$06, $00,$0C,$18, $00,$00,$00


BootAnimationColors:
    rgb $1F, $1F, $1F ; White
    rgb $00, $00, $1F ; Blue
    rgb $00, $1F, $00 ; Green
    rgb $1F, $00, $1F ; Purple
    rgb $1F, $00, $00 ; Red
    rgb $1F, $1F, $00 ; Yellow
.end


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
.end


JoypadCombosTripletIDsAndFlags:
    idx_flgs %000,18
    idx_flgs %101,16
    idx_flgs %011,25

    idx_flgs %101,24
    idx_flgs %101,13
    idx_flgs %000,22

    idx_flgs %000,23
    idx_flgs %000, 7
    idx_flgs %101,26

    idx_flgs %000, 5
    idx_flgs %011,28
    idx_flgs %000,19

    ds 4, 0 ; 4 unused bytes



SECTION "VRAM tiles 0", VRAM[_VRAM], BANK[0]

vBlankTile:
    ds TILE_SIZE
vLogoTiles:
    ds  (HeaderTitle - HeaderLogo) * TILE_SIZE / 2
vRTile:
    ds TILE_SIZE

SECTION "VRAM tiles 1", VRAM[_VRAM], BANK[1]

vTiles:
    ds 8 * TILE_SIZE

vGameBoyLogoTiles:
    ds (GameBoyLogoTiles.end - GameBoyLogoTiles) * 4
vNintendoLogoTiles:
    ds 6 * TILE_SIZE
vSecondRTile:
    ds TILE_SIZE
vNintendoLogoTilesEnd:

;; Definition of VRAM layout

MACRO vram_block
    ; Y rows + X columns
    SECTION "\1 tilemap", VRAM[_SCRN0 + SCRN_VX_B * (\3) + (\2)], BANK[0]
    \1Map:
    SECTION "\1 attrmap", VRAM[_SCRN0 + SCRN_VX_B * (\3) + (\2)], BANK[1]
    \1Attrs:
ENDM

; Actual definition...

SECTION "Tilemap", VRAM[_SCRN0], BANK[0]
vTileMap:
SECTION "Attrmap", VRAM[_SCRN0], BANK[1]
vAttrMap:

    vram_block vGameBoyLogo, 2, 6, GB_LOGO_WIDTH, GB_LOGO_HEIGHT
    vram_block vNintendoLogo, 7, 13, vNintendoLogoTilesEnd - vNintendoLogoTiles + 1, 1 ; +1 for the "®"

    vram_block vBigNintendoLogo, 4, 8, OLD_LOGO_WIDTH, OLD_LOGO_HEIGHT


SECTION "Work RAM", WRAMX[_RAMBANK], BANK[2]
wWorkRAM:

wTitleChecksum:
    ds 1

    ds 1

wPreventTerminationCounter:
    ds 1

wHeldButtons:
    ds 1
wPressedButtons: ; This is never read, only written to
    ds 1

wPaletteOverrideIndex:
    ds 1

wWhichPalTripletCopy:
    ds 1
wOldWhichPalTriplet:
    ds 1
wWhichPalTriplet:
    ds 1
wPalShufflingFlagsCopy: ; This is only written to by the CGB0 boot ROM. Debugging purposes?
    ds 1
wOldPalShufflingFlags: ; This saves the previous value of wPalShufflingFlags, maybe for Debugging purposes?
    ds 1
; Explanation at WriteShuffledPalTriplets
wPalShufflingFlags:
    ds 1

    ds $2F4

wZeroBuffer:
    ds $400

    ds $100

wOBJPalBuffer:
    ds 8 * 8
.end

wBGPalBuffer:
    ds 8 * 8
.end

    ds $80

; The 3 palette offsets are written here for all triplets, modified by the "shuffling flags"
; For each offset triplet written here, the order is as follows: OBP0, OBP1, BGP
wPalOfsBuffer:
    ds 3 * 30
.end
    ds 3 * 2
.realEnd

    ds $A0

wPalBuffer:
    ds 96 * 8


SECTION "HRAM", HRAM[_HRAM]

hLogoBuffer: ; Relied on being at $FF80
    ds $7E

hStackBottom:
