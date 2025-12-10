DAYS := $(shell printf '%s\n' [0-9]* | sort -n)

.PHONY: all $(DAYS)

all: $(DAYS)

$(DAYS):
	$(MAKE) -C $@
