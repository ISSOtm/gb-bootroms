
.SECONDEXPANSION:


MODELS := dmg0 dmg mgb sgb sgb2 cgb0 cgb agb0 agb stadium2

dmg0_asm  := dmg.asm
dmg0_size := 256
dmg_asm   := dmg.asm
dmg_size  := 256
mgb_asm   := dmg.asm
mgb_size  := 256

sgb_asm   := sgb.asm
sgb_size  := 256
sgb2_asm  := sgb.asm
sgb2_size := 256

cgb0_asm  := cgb.asm
cgb0_size := 2304
cgb_asm   := cgb.asm
cgb_size  := 2304
agb0_asm  := cgb.asm
agb0_size := 2304
agb_asm   := cgb.asm
agb_size  := 2304

stadium2_asm  := stadium2.asm
stadium2_size := 1008


compare: sha256sums.txt all
	sha256sum -c $<
.PHONY: compare

BINS := $(patsubst %,bin/%.bin,${MODELS})
SYMS := $(patsubst %,bin/%.sym,${MODELS})
all: ${BINS} ${SYMS}
.PHONY: all

clean:
	rm -rf bin obj
.PHONY: clean

# `/dev/stdout` is emulated by Bash if not provided by the system.
# Also, this pipeline won't fail if `rgblink` errors out but `sed` doesn't.
# But `set -o pipefail` would fail to build on Alpine or w/e, so let's assume linking will go well.
bin/%.sym bin/%.bin: obj/%.o
	@mkdir -p ${@D}
	rgblink -p 0 $^ -o bin/$*.bin -n /dev/stdout | sed 's/^0*:0/BOOT:0/' >bin/$*.sym
	truncate -s ${$*_size} bin/$*.bin

obj/%.o: src/$${$$*_asm}
	@mkdir -p ${@D}
	rgbasm -p 0xFF -D $* -I src/ -o $@ $<
