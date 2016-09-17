WINDRES ?= windres
RCFLAGS ?=

%.o: %.rc
	$(WINDRES) $(RCFLAGS) $< -o $@
