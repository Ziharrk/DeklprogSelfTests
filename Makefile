FONTS_DIR = fonts
CODE_FONT_URL = https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/CascadiaCode.zip
CODE_FONT_ZIP = $(FONTS_DIR)/CascadiaCode.zip
CODE_FONTS = $(FONTS_DIR)/CaskaydiaCoveNerdFont*.ttf

SYNTAXES_DIR = syntaxes

all: main.pdf

main.pdf: main.typ $(FONTS_DIR)/.fonts-extracted syntaxes/prolog.sublime-syntax
	typst compile --font-path $(FONTS_DIR) main.typ

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

clean:
	rm -rf main.pdf $(FONTS_DIR)

