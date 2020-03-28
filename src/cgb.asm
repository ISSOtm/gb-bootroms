INCLUDE "hardware.inc/hardware.inc"
INCLUDE "header.inc"

; Official Nintendo names for those registers
; Obtained from the archive leaked here: http://boards.4channel.org/vp/thread/43077976
; Translation of the document's register descriptions will follow

; CPU mode select
; XXXXmmXX
; 00: CGB mode (for execution of CGB supporting cartridges)
; 01: DMG/MGB mode (for execution of DMG/MGB exclusive cartridges)
; 10: PGB1 mode (a mode in which the CPU is stopped and the LCD is driven externally)
; 11: PGB2 mode (a mode in which the LCD is driven externally while the CPU is operating)
rKEY0 equ $FF4C ; Side note: the register's name is consistent with rKEY1 at $FF4D
; Internal/external rom bank register
; XXXXXXXr
; 0: monitor ROM, 1: cassette ROM
rBANK equ $FF50
; OBJ priority mode designation register
; XXXXXXXp
; 0: Lower OBJ-NO object priority
; 1: Smaller X coordinate object priority
rOPRI equ $FF6C

; KEY0 is known as the "CPU mode register" in Fig. 11 of this patent:
; https://patents.google.com/patent/US6322447B1/en?oq=US6322447bi
; "OBJ priority mode designating register" in the same patent
; Credit to @mattcurrie for this finding!


COLOR_MASK equ $1F ; Each color spans 5 bits
COLOR_MAX equ $1F
COLOR_SIZE equ 2 ; Colors are encoded as 2-byte little-endian BGR555
COLORS_PER_PALETTE equ 4
PALETTE_SIZE equ 8

TILE_SIZE equ 16

NB_OBJS equ 40 ; OAM contains 40 OBJs
OBJ_SIZE equ 4 ; Each OAM OBJ is 4 bytes
OAM_SIZE equ NB_OBJS * OBJ_SIZE

WAVE_RAM_SIZE equ 16


; "GAME BOY" logo constants
GB_LOGO_HEIGHT equ 3 ; The logo is 3 tile rows tall
GB_LOGO_WIDTH equ 16 ; The logo is 12 tiles wide
LOGO_BLOCK_WIDTH equ 3 ; Width in tiles of one color "block"
NB_LOGO_PALETTES equs "((BootAnimationColorsEnd - BootAnimationColors) / COLOR_SIZE)"
GB_LOGO_FIRST_TILE equs "LOW(vGameBoyLogoTiles / TILE_SIZE)"

; Old "Nintendo" logo constants
OLD_LOGO_HEIGHT equ 2
OLD_LOGO_WIDTH equ 12


;; Definition of VRAM layout

; Helper macros

NB_VRAM_BLOCKS = 0
; Args: name, x, y, width, height
; Will define `v<name>Map` for the tilemap, and `v<name>Attr` for the attr map
vram_block: MACRO
VRAM_BLOCK_NAME equs "VRAM_BLOCK{d:NB_VRAM_BLOCKS}"
VRAM_BLOCK_NAME equs "ds (\2) + (\3) * SCRN_VX_B\nv\1\\1:\nds (\4) + (\5) * SCRN_VX_B"
    PURGE VRAM_BLOCK_NAME
NB_VRAM_BLOCKS = NB_VRAM_BLOCKS + 1
ENDM
; Arg: either `Map' for the tilemap, or `Attr' for the attr map
; Call within an UNION
vram_blocks: MACRO
BLOCK_ID = 0
    REPT NB_VRAM_BLOCKS
        NEXTU
VRAM_BLOCK_NAME equs "VRAM_BLOCK{d:BLOCK_ID}"
            VRAM_BLOCK_NAME
            PURGE VRAM_BLOCK_NAME
BLOCK_ID = BLOCK_ID + 1
    ENDR
    PURGE BLOCK_ID
ENDM

; Actual definition...

    vram_block GameBoyLogo, 2, 6, GB_LOGO_WIDTH, GB_LOGO_HEIGHT
    vram_block NintendoLogo, 7, 13, vNintendoLogoTilesEnd - vNintendoLogoTiles + 1, 1 ; +1 for the "®"

    vram_block BigNintendoLogo, 4, 8, OLD_LOGO_WIDTH, OLD_LOGO_HEIGHT


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
    ; Clear 9 rows starting at the one above the "GAME BOY" logo
    gdma_params wZeroBuffer, (vGameBoyLogoMap & -SCRN_VX_B) - SCRN_VX_B, SCRN_VX_B * 9
ClearLogoGDMAEnd:
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
; The boot ROM only checks for the top half, but the full logo is present anyways
; for the two games that need it displayed
LogoBottomHalf:
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
RTileEnd:


; Titles matching these checksums need the Nintendo logo tilemap in VRAM when they boot up
LogoTilemapChecksums:
    db $58, $43
LogoTilemapChecksumsEnd:


Setup:
    ldh [rSVBK], a
    ld a, $FC ; Colors: 3 3 3 0
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

    ld a, BANK(vGameBoyLogoAttr)
    ldh [rVBK], a
    ld a, LCDCF_ON | LCDCF_BG8000 | LCDCF_BGON
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
    ldh [rBANK], a


SECTION "Second section", ROM0[$200]

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
    ld e, wBGPalBufferEnd - wBGPalBuffer
    call SetOBJAndBGPals
    pop hl
    pop de
    pop bc
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
    xor a ; ld a, BANK(vMainTilemap)
    ldh [rVBK], a
    ld hl, vNintendoLogoMap
    ld a, LOW(vNintendoLogoTiles / TILE_SIZE)
.writeNintendoLogoMap
    ld [hli], a
    inc a
    cp LOW(vNintendoLogoTilesEnd / TILE_SIZE)
    jr nz, .writeNintendoLogoMap
    ld a, BANK(vMainAttrMap)
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
    cp LOW(vGameBoyLogoAttr + GB_LOGO_WIDTH - 1)
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
    call CommitBGPalettes
    dec c
    jr nz, .loop
    call WaitVBlank
    ld a, BANK(vMainAttrMap)
    ldh [rVBK], a
    call .clearLogoArea
    ; ld hl, ClearLogoTilesGDMA
    call .clearLogoTiles
    xor a ; ld a, BANK(vMainTilemap)
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
    ld b, (GameBoyLogoTilesEnd - GameBoyLogoTiles) / 4
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

    ld hl, vGameBoyLogoAttr
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
    cp TitleChecksumsEnd - TitleChecksums
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
    sub (TitleFourthLettersEnd - TitleFourthLetters) \ ; Max index
      + (TitleChecksums.ambiguous - TitleChecksums) ; (not forgetting base index)
    ; If there's some indexes left to check, keep going
    jr c, .seekFourthLetter
    ; If not, use default index

.useDefaultIndex
    ld c, 0
.gotIndex
    ld hl, PaletteIndexesAndFlags
    ld b, 0
    add hl, bc
    ld a, [hl]
    and $1F
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
    call WriteShuffledPalTriplets
    ret

; "bit N" refers to the Nth bit in wPalShufflingFlags
;
; Shuffles predefined palette triplets as such:
; - 1st entry (OBP0) is set to 3rd elem if bit 0 is set, otherwise 1st
; - 2nd entry (OBP1) is set to 2nd elem if bit 2 is set, otherwise 3rd if bit 1 is set, otherwise 1st
; - 3rd entry (BGP)  is set to 3rd elem always
WriteShuffledPalTriplets:
    ld de, PaletteIndexes
    ld hl, wPalIndexBuffer
    ld a, [wPalShufflingFlags]
    ld b, a
    ld c, (wPalIndexBufferEnd - wPalIndexBuffer) / 3 ; = 90 / 3 = 30
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
    ld hl, wWhichPalTriplet
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
    ld a, $04
    ldh [rKEY0], a
    ld a, 1
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
    ld c, LogoTilemapChecksumsEnd - LogoTilemapChecksums
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
.ambiguous
    ; These checksums are also discriminated based on the 4th title letter
    db $B3, $46, $28, $A5, $C6, $D3, $27, $61, $18, $66, $6A, $BF, $0D, $F4
TitleChecksumsEnd:

    ; To explain this: letters for index $41 are the leftmost ones, ie.
    ; B, U and R in order.
TitleFourthLetters:
; $4x:  123456789ABCDE
    db "BEFAARBEKEK R-"
.row
    db "URAR INAILICE "
    db "R"
TitleFourthLettersEnd:


PaletteIndexesAndFlags:
    ; TODO: improve this dump
    db $7C, $08, $12, $A3, $A2, $07, $87, $4B, $20, $12, $65, $A8, $16, $A9, $86, $B1, $68, $A0, $87, $66, $12, $A1, $30, $3C, $12, $85, $12, $64, $1B, $07, $06, $6F, $6E, $6E, $AE, $AF, $6F, $B2, $AF, $B2, $A8, $AB, $6F, $AF, $86, $AE, $A2, $A2, $12, $AF, $13, $12, $A1, $6E, $AF, $AF, $AD, $06, $4C, $6E, $AF, $AF, $12, $7C, $AC, $A8, $6A, $6E, $13, $A0, $2D, $A8, $2B, $AC, $64, $AC, $6D, $87, $BC, $60, $B4, $13, $72, $7C, $B5, $AE, $AE, $7C, $7C, $65, $A2, $6C, $64, $85

PaletteIndexes:
    ; BGP will always be the third entry
    ; OBP0 can be either the first or last entry
    ; OBP1 can be any entry
    db $80, $B0, $40
    db $88, $20, $68
    db $DE, $00, $70
    db $DE, $20, $78
    db $20, $20, $38
    db $20, $B0, $90
    db $20, $B0, $A0
    db $E0, $B0, $C0
    db $98, $B6, $48
    db $80, $E0, $50
    db $1E, $1E, $58
    db $20, $B8, $E0
    db $88, $B0, $10
    db $20, $00, $10
    db $20, $E0, $18
    db $E0, $18, $00
    db $18, $E0, $20
    db $A8, $E0, $20
    db $18, $E0, $00
    db $20, $18, $D8
    db $C8, $18, $E0
    db $00, $E0, $40
    db $28, $28, $28
    db $18, $E0, $60
    db $20, $18, $E0
    db $00, $00, $08
    db $E0, $18, $30
    db $D0, $D0, $D0
    db $20, $E0, $E8

Palettes:
    rgb $1F,$1F,$1F, $1F,$15,$0C, $10,$06,$00, $00,$00,$00
    rgb $1F,$1C,$18, $19,$13,$10, $10,$0D,$05, $0B,$06,$01
    rgb $1F,$1F,$1F, $11,$11,$1B, $0A,$0A,$11, $00,$00,$00
    rgb $1F,$1F,$1F, $0F,$1F,$06, $00,$10,$00, $00,$00,$00
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
    rgb $1F,$1F,$1F, $0B,$17,$1F, $1F,$00,$00, $00,$00,$1F
    rgb $1F,$1F,$1F, $1F,$1F,$0F, $00,$10,$1F, $1F,$00,$00
    rgb $1F,$1F,$1F, $1F,$1F,$00, $1F,$00,$00, $00,$00,$00
    rgb $1F,$1F,$00, $1F,$00,$00, $0C,$00,$00, $00,$00,$00
    rgb $1F,$1F,$1F, $1F,$19,$00, $13,$0C,$00, $00,$00,$00
    rgb $00,$00,$00, $00,$10,$10, $1F,$1B,$00, $1F,$1F,$1F
    rgb $1F,$1F,$1F, $0C,$14,$1F, $00,$00,$1F, $00,$00,$00
    rgb $1F,$1F,$1F, $0F,$1F,$06, $00,$0C,$18, $00,$00,$00


BootAnimationColors:
    rgb $1F, $1F, $1F ; White
    rgb $00, $00, $1F ; Blue
    rgb $00, $1F, $00 ; Green
    rgb $1F, $00, $1F ; Purple
    rgb $1F, $00, $00 ; Red
    rgb $1F, $1F, $00 ; Yellow
BootAnimationColorsEnd:


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
vTiles:

    ds $80

vGameBoyLogoTiles:
    ds (GameBoyLogoTilesEnd - GameBoyLogoTiles) * 4
vNintendoLogoTiles:
    ds 6 * 16
vSecondRTile:
    ds 16
vNintendoLogoTilesEnd:

SECTION "Tilemap", VRAM[$9800],BANK[0]

UNION
vMainTilemap:
    ds SCRN_VX_B * SCRN_VY_B
    vram_blocks Map
ENDU

SECTION "Attr map", VRAM[$9800],BANK[1]

UNION
vMainAttrMap:
    ds SCRN_VX_B * SCRN_VY_B
    vram_blocks Attr
ENDU


SECTION "Work RAM", WRAMX[$D000],BANK[2]
wWorkRAM:

wTitleChecksum:
    ds 1

    ds 1

wPreventTerminationCounter:
    ds 1

wHeldButtons:
    ds 1
wPressedButtons: ; This is never read, only
    ds 1

wPaletteOverrideIndex:
    ds 1

    ; TODO: some bytes around WhichPalTriplet and D00B are used in specific circumstances, investigate and label them
wWhichPalTripletCopy:
    ds 1
wOldWhichPalTriplet:
    ds 1
wWhichPalTriplet:
    ds 1
wPalShufflingFlagsCopy: ; This is only written to by the CGB0 boot ROM. Debugging purposes?
    ds 1
wOldPalShufflingFlags: ; This saves the previousvalue of wPalShufflingFlags, maybe for Debugging purposes?
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
wOBJPalBufferEnd:

wBGPalBuffer:
    ds 8 * 8
wBGPalBufferEnd:

    ds $80

wPalIndexBuffer:
    ds 90
wPalIndexBufferEnd:
    ds 6
wPalIndexBufferRealEnd:

    ds $A0

wPalBuffer:
    ds 96 * 8


SECTION "High RAM", HRAM[$FF80]

hLogoBuffer: ; Relied on being at $FF80
    ; ds ?
    ds $7E

hStackBottom:
