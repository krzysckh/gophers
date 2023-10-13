OL=ol
TARGET=gophers

CFLAGS=-O3
OLFLAGS=-O2

.SUFFIXES: .scm .c

all: gophers.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(TARGET) gophers.c
.scm.c:
	$(OL) $(OLFLAGS) -x c -o $@ $<
clean:
	rm -f gophers.c $(TARGET)
