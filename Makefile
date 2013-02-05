CC := ocamlbuild
SRC := src
LIB := lib
CFLAGS := -I $(SRC) -I $(LIB)

.PHONY: all mproper

all: main generator

main:
	$(CC) $(CFLAGS) $(SRC)/$@.native

generator:
	$(CC) $(CFLAGS) $(SRC)/$@.native

clean:
	$(CC) -clean

