CC := ocamlbuild
SRC := .
CFLAGS := 

.PHONY: all mproper

all: main 

main:
	$(CC) $(CFLAGS) $(SRC)/$@.native

clean:
	$(CC) -clean

