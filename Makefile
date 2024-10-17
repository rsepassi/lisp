lisp: lisp.c
	cc -o $@ lisp.c

lisp.o:
	cc -o $@ -DNOMAIN -c lisp.c

liblisp.a: lisp.o
	ar rcs liblisp.a $^

clean:
	rm -f lisp liblisp.a lisp.o
