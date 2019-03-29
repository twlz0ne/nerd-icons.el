EMACS ?= emacs
ELS = nerd-icons.el nerd-icons-data.el nerd-icons-faces.el
ELCS = nerd-icons.elc nerd-icons-data.elc nerd-icons-faces.elc
TEST_ELS = test-nerd-icons.el

# If the first argument is "test"...
ifeq (test, $(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "test"
  SELECTOR := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(SELECTOR):;@:)
endif

all: clean compile test

%.elc:%.el
	$(EMACS) -batch -L . -f batch-byte-compile $(ELS)

compile:$(ELCS)

.PHONY: test

test:$(ELCS)
ifeq ($(SELECTOR),)
	$(EMACS) -Q --batch -L . $(addprefix -l , $(ELCS) $(TEST_ELS)) -f ert-run-tests-batch-and-exit
else
	$(EMACS) -Q --batch -L . $(addprefix -l , $(ELCS) $(TEST_ELS)) --eval "(ert-run-tests-batch-and-exit '$(SELECTOR))"
endif

help:
	@echo make
	@echo make compile
	@echo make test [SELECTOR]
	@echo make clean

clean:
	@rm -f *.elc
