LANGUAGES := 2020-elisp 2025-scheme

.PHONY: all $(LANGUAGES)

all: $(LANGUAGES)

$(LANGUAGES):
	$(MAKE) -C $@
