LIB=../priv/enit_posix.so

all: $(LIB) 

$(LIB) : enit_posix.c
	$(CC) -std=gnu99 -g -O0 -Wall -shared -fpic -o $@ $^

clean:
	rm -f $(LIB) 

