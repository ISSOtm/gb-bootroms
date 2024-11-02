IF !DEF(HEADER_INC)
DEF HEADER_INC EQU 1

MACRO header_section
    PUSHS
    SECTION "\1", ROM0[\2]
    \1:
    POPS
ENDM

	header_section HeaderLogo,           $0104
	header_section HeaderTitle,          $0134
	header_section HeaderMenufacturer,   $013F
	header_section HeaderCGBCompat,      $0143
	header_section HeaderNewLicensee,    $0144
	header_section HeaderSGBFlag,        $0146
	header_section HeaderCartType,       $0147
	header_section HeaderROMSize,        $0148
	header_section HeaderRAMSize,        $0149
	header_section HeaderRegionCode,     $014A
	header_section HeaderOldLicensee,    $014B
	header_section HeaderROMVersion,     $014C
	header_section HeaderChecksum,       $014D
	header_section HeaderGlobalChecksum, $014E

ENDC
