FONTS_DIR = fonts
CODE_FONT_URL = https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/CascadiaCode.zip
CODE_FONT_ZIP = $(FONTS_DIR)/CascadiaCode.zip
CODE_FONTS = $(FONTS_DIR)/CaskaydiaCoveNerdFont*.ttf

SYNTAXES_DIR = syntaxes

.PHONY: src/index.json templates.zip solutions.zip

all: main.pdf templates.zip solutions.zip

main.pdf: src/* src/index.json $(FONTS_DIR)/.fonts-extracted syntaxes/prolog.sublime-syntax
	typst compile --root . --font-path $(FONTS_DIR) --input "now=$(shell date '+%Y %m %d %H %M %S')" src/main.typ main.pdf

syntaxes/prolog.sublime-syntax:
	mkdir -p syntaxes
	git clone https://github.com/BenjaminSchaaf/swi-prolog-sublime-syntax.git syntaxes/swi-prolog-sublime-syntax
	make -C syntaxes/swi-prolog-sublime-syntax 
	mv syntaxes/swi-prolog-sublime-syntax/Prolog/SWI-Prolog.sublime-syntax syntaxes/prolog.sublime-syntax
	rm -rf syntaxes/swi-prolog-sublime-syntax

$(FONTS_DIR)/.fonts-extracted: $(CODE_FONT_ZIP)
	unzip $(CODE_FONT_ZIP) "*.ttf" -d $(FONTS_DIR)
	touch $@

$(CODE_FONT_ZIP):
	mkdir -p $(FONTS_DIR)
	wget -O $(CODE_FONT_ZIP) $(CODE_FONT_URL)

src/index.json:
	python src/index.py

templates.zip solutions.zip:
	python src/archive.py

clean:
	rm -rf main.pdf $(FONTS_DIR)
	rm -rf src/index.json

