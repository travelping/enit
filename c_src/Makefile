PRIVDIR := ../priv

LD ?= ld
CC ?= gcc
RM ?= rm

CFLAGS ?= -O2

override CFLAGS += -std=gnu99 -Wall -fpic
override LDFLAGS += -shared -fpic

all : $(PRIVDIR)/enit_posix.so

$(PRIVDIR)/enit_posix.so : enit_posix.o
	$(CC) $(LDFLAGS) -o $@ $^

clean:
	$(RM) -f $(PRIVDIR)/enit_posix.so enit_posix.o

