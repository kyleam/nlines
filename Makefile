EMACS = emacs -Q --batch
name = nlines
main_el := $(name).el
main_elc = $(main_el)c
AUTOLOADS_FILE := $(name)-autoloads.el

all: $(AUTOLOADS_FILE) $(main_elc)

$(AUTOLOADS_FILE): $(main_el)
	@$(EMACS) --eval \
	"(let (make-backup-files) \
	  (update-file-autoloads \"$(CURDIR)/$<\" t \"$(CURDIR)/$@\"))"

%.elc: %.el
	@$(EMACS) -f batch-byte-compile $<

.PHONY: clean
clean:
	$(RM) $(main_elc) $(AUTOLOADS_FILE)
