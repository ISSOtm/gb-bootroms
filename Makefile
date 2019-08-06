
.SECONDEXPANSION:


MODELS = dmg0 dmg mgb sgb sgb2 cgb0 cgb agb

dmg0_asm  = dmg.asm
dmg0_size = 256
dmg_asm   = dmg.asm
dmg_size  = 256
mgb_asm   = dmg.asm
mgb_size  = 256

sgb_asm   = sgb.asm
sgb_size  = 256
sgb2_asm  = sgb.asm
sgb2_size = 256

cgb0_asm  = cgb.asm
cgb0_size = 2304
cgb_asm   = cgb.asm
cgb_size  = 2304
agb_asm   = cgb.asm
agb_size  = 2304


compare: sha256sums.txt all
	sha256sum -c $<
.PHONY: compare

all: $(patsubst %,bin/%.bin,$(MODELS))
.PHONY: all

clean:
	rm -rf bin obj
.PHONY: clean

bin/%.bin: obj/%.o
	@mkdir -p $(@D)
	rgblink -p 0 -o $@ $^
	truncate -s $($*_size) $@

obj/%.o: src/$$($$*_asm)
	@mkdir -p $(@D)
	rgbasm -p 0xFF -D $* -i src/ -o $@ $<
