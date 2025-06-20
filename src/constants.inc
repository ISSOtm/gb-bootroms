IF !DEF(CONSTANTS_INC)
DEF CONSTANTS_INC EQU 1

DEF COLOR_MASK equ $1F ; Each color spans 5 bits
DEF COLOR_MAX equ $1F
DEF COLOR_SIZE equ 2 ; Colors are encoded as 2-byte little-endian BGR555
DEF COLORS_PER_PALETTE equ 4
DEF PALETTE_SIZE equ COLOR_SIZE * COLORS_PER_PALETTE

DEF TILE_SIZE equ 16 ; Each tile is 16 bytes = 8x8 pixels * 2 bits/pixel

DEF NB_OBJS equ 40 ; OAM contains 40 OBJs
DEF OBJ_SIZE equ 4 ; Each OAM OBJ is 4 bytes
DEF OAM_SIZE equ NB_OBJS * OBJ_SIZE

DEF WAVE_RAM_SIZE equ 16

; "GAME BOY" logo constants
DEF GB_LOGO_HEIGHT equ 3 ; The logo is 3 tile rows tall
DEF GB_LOGO_WIDTH equ 16 ; The logo is 12 tiles wide
DEF LOGO_BLOCK_WIDTH equ 3 ; Width in tiles of one color "block"
DEF NB_LOGO_PALETTES equs "((BootAnimationColors.end - BootAnimationColors) / COLOR_SIZE)"
DEF GB_LOGO_FIRST_TILE equs "LOW(vGameBoyLogoTiles / TILE_SIZE)"

; Old "Nintendo" logo constants
DEF OLD_LOGO_HEIGHT equ 2
DEF OLD_LOGO_WIDTH equ 12

MACRO gdma_params
    db HIGH(\1), LOW(\1)
    db HIGH(\2), LOW(\2)
    db (\3) / 16
ENDM

MACRO rgb
    REPT _NARG / 3
        dw ((\1) & COLOR_MASK) | (((\2) & COLOR_MASK) << 5) | (((\3) & COLOR_MASK) << 10)
        SHIFT 3
    ENDR
ENDM

; Each tile is encoded using 2 (!) bytes
; The logo is split into two halves (top and bottom), each half being encoded separately.
; Each half must be read in columns.
MACRO logo_row_gfx_nybbles
    ASSERT _NARG % 4 == 0
    PUSHO
    OPT b.X
    FOR N1, 1, _NARG / 4 + 1 ; N1, N2, N3, and N4 iterate through the 4 equally-sized rows
        DEF N2 = N1 + _NARG / 4
        DEF N3 = N2 + _NARG / 4
        DEF N4 = N3 + _NARG / 4
        db %\<N1>\<N2>, %\<N3>\<N4>
    ENDR
    POPO
ENDM

; Each row is 1bpp (different colors are achieved through attrmap modifications), halving the size.
; And then the logo is upscaled vertically (doubled), halving yet again.
MACRO logo_row_gfx_bytes
    ASSERT _NARG % 4 == 0
    PUSHO
    OPT b.X
    FOR N1, 1, _NARG / 4 + 1 ; N1, N2, N3, and N4 iterate through the 4 equally-sized rows
        DEF N2 = N1 + _NARG / 4
        DEF N3 = N2 + _NARG / 4
        DEF N4 = N3 + _NARG / 4
        db %\<N1>, %\<N2>, %\<N3>, %\<N4>
    ENDR
    POPO
ENDM

ENDC
