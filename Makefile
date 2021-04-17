NAME = dodge-the-creeps

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    LIBNAME = lib$(NAME).so
endif
ifeq ($(UNAME_S),Darwin)
    LIBNAME = lib$(NAME).dylib
endif

STACKLIBFILE = $(shell stack path --local-install-root)/lib/$(LIBNAME)
GODOTPROJECT = $(shell stack path --project-root)/game
all: stack
stack:
	stack clean $(NAME)
	stack build
	cp $(STACKLIBFILE) $(GODOTPROJECT)/lib
stack-run:
	stack clean $(NAME)
	stack build
	cp $(STACKLIBFILE) $(GODOTPROJECT)/lib
	godot --path ./game
stack-watch:
	stack build --fast --file-watch --exec "cp $(STACKLIBFILE) $(GODOTPROJECT)/lib"
