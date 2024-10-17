CFLAGS = -Wall -Werror -pedantic -std=c89 -static -nostdlib -fno-asynchronous-unwind-tables -fno-stack-protector

lisp: lisp.c
	$(CC) -o $@ $(CFLAGS) $^
	strip -s -R .comment $@

lisp.o: lisp.c
	$(CC) -o $@ -DNOMAIN $(CFLAGS) -c lisp.c

liblisp.a: lisp.o
	$(AR) rcs liblisp.a $^

clean:
	rm -f lisp liblisp.a lisp.o
