files=$(shell find public -maxdepth 1 -type f)
out=$(foreach f,$(files),build/$(notdir $(f)))

all: build/js/app.js $(out)

build/js/app.js: $(shell find src -type f)
	lein package

build/%: public/%
	@mkdir -p build
	cp $< $@

.PHONY: clean

clean:
	rm -rf build/js
	rm -f build/*
