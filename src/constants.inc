IF !DEF(CONSTANTS_INC)
DEF CONSTANTS_INC EQU 1

; Official Nintendo names for those registers
; Obtained from the archive leaked here: http://boards.4channel.org/vp/thread/43077976
; Translation of the document's register descriptions will follow

; -- CPU mode select
; KEY0 is known as the "CPU mode register" in Fig. 11 of this patent:
; https://patents.google.com/patent/US6322447B1/en?oq=US6322447bi
; "OBJ priority mode designating register" in the same patent
; Credit to @mattcurrie for this finding!
DEF rKEY0 equ $FF4C ; Side note: the register's name is consistent with rKEY1 at $FF4D
DEF KEY0F_MODE equ %00001100
DEF KEY0_CGB  equ %00000000 ; CGB mode (for execution of CGB supporting cartridges)
DEF KEY0_DMG  equ %00000100 ; DMG/MGB mode (for execution of DMG/MGB exclusive cartridges)
DEF KEY0_PGB1 equ %00001000 ; PGB1 mode (a mode in which the CPU is stopped and the LCD is driven externally)
DEF KEY0_PGB2 equ %00001100 ; PGB2 mode (a mode in which the LCD is driven externally while the CPU is operating)

; -- Internal/external rom bank register
DEF rBANK equ $FF50
DEF rBANKF_ROM equ %00000001 ; 0: monitor ROM, 1: cassette ROM


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
DEF NB_LOGO_PALETTES equs "((BootAnimationColors.end - BootAnimationColors) / COLOR_SIZE)"
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

ENDC
