/* A Lisp. */

/* L, a Lisp value, represented as an int. */
#define L int

/* Our Lisp exports.
 *
 * init:
 *   Initializes the Lisp runtime. Must be called exactly once before calls to
 *   parse, eval, or print.
 * parse:
 *   Parses a string Lisp expression into a Lisp tree of atoms.
 * eval:
 *   Evaluates a parsed Lisp expression to a Lisp value.
 * print:
 *   Prints a Lisp value. Expects putchar to be defined.
 */
void init();
L parse(char **s);
L eval(L expr, L env);
void print(L x);

/* Our simple output mechanism. */
int putchar(int);

#ifndef NOMAIN
/* main.
 *
 * Takes a Lisp program composed of 0 or more Lisp expressions and
 * parse,eval,prints each one.
 */
int main(int argc, char **argv) {
  char *program;
  init();
  program = argv[1];
  while (*program) {
    print(eval(parse(&program), 0));
    putchar('\n');
  }
  return 0;
}

#if defined(__linux__) && defined(__x86_64__)
/* Define our own _start and putchar so that we don't depend on libc.
 * Note: These are Linux x86-64 specific.
 */
#define SYS_write 1
#define SYS_exit 60
#define stdout 1
__attribute__((naked)) void _start(void) {
  __asm__ (
      "mov (%%rsp), %%rdi\n"   /* argc */
      "lea 8(%%rsp), %%rsi\n"  /* argv */
      "and $-16, %%rsp\n"      /* align stack to 16 bytes */
      "call main\n"
      "mov %%rax, %%rdi\n"     /* move return value to rdi for exit status */
      "mov $%c0, %%rax\n"      /* SYS_exit */
      "syscall\n"
      "ret"                    /* unreachable */
      : /* No outputs */
      : "i" (SYS_exit)
      : "rdi", "rsi", "rax"    /* clobbered registers */
  );
}
void* syscall3(void* number, void* arg1, void* arg2, void* arg3) {
  void* ret;
  __asm__ (
      "mov %1, %%rax\n"
      "mov %2, %%rdi\n"
      "mov %3, %%rsi\n"
      "mov %4, %%rdx\n"
      "syscall"
      : "=a" (ret)
      : "g" (number), "g" (arg1), "g" (arg2), "g" (arg3)
      : "rcx", "r11", "memory"
  );
  return ret;
}
int putchar(int c) {
  /* write(1, &c, 1); */
  syscall3(
      (void*)SYS_write,
      (void*)stdout,
      (void*)&c,
      (void*)1);
  return c;
}
#endif  /* __linux__ */
#endif  /* NOMAIN */

/* Implementation */
/* ------------------------------------------------------------------------- */

/* Lisp runtime state.
 *
 * ATOM:
 *   During parsing, all string atoms are interned as 0-terminated strings
 *   here. L values <0 are atoms and are indices into ATOM via (-i - 1).
 * CELL:
 *   All Lisp values that are not an atom or nil are "cons" cells, pairs of
 *   Lisp values (car, cdr). L values >0 are cells and are indices into CELL
 *   via car=(i - 2) cdr=(i - 1).
 * atom_next:
 *   Points to the current end of the ATOM array.
 * p_end:
 *   Primitive names are interned in ATOM first. This points to the end of the
 *   primitive names to help quickly determine whether a given atom is a
 *   primitive.
 * cell_next:
 *   Points to the current end of the CELL array.
 * genv:
 *   Top-level global environment. It initially contains only the primitives.
 *   Calls to define extend it.
 */
char ATOM[1 << 20];
L CELL[1 << 20];
char *atom_next;
char *p_end;
L *cell_next;
L genv;

/* These are the delimiters used for parsing.
 *
 * And newline and semicolon are just niceties to allow for line breaks and
 * comments. Lisp is a tiny language.
 */
#define LPAREN '('
#define RPAREN ')'
#define SPACE ' '
#define NEWLINE '\n'
#define SEMICOLON ';'

/* All Lisp values L are one of 3 types: nil, atom, or cell. */
#define T_nil 0
#define T_atom 1
#define T_cell 2

/* Lisp primitives.
 *
 * There are exactly 10 primitives plus nil.
 * These are the only predefined values in the language.
 *
 * Each is defined as an atom and has an associated function that is used for
 * evaluation. P_x are the primitives' L values.
 *
 * ():
 *   nil, "false", empty cell.
 * #t:
 *   "true", an atom that is simply not nil. It is never compared against or
 *   used for anything except to signify "not nil". Not strictly necessary, but
 *   useful.
 * (quote x):
 *   Returns x without evaluating it.
 * (atom x):
 *   #t if x is an atom or nil,
 *   nil otherwise.
 * (eq a b):
 *   #t if a and b are both nil, or if a and b are the same atom,
 *   nil otherwise.
 * (car x):
 *   Returns the head of the cell.
 * (cdr x):
 *   Returns the tail of the cell.
 * (cons head tail):
 *   Creates a cell.
 * (cond (c1 e1) ... (cn en)):
 *   Evaluates ci in order until one evaluates to not-nil. Returns the
 *   corresponding ei.
 * (lambda (p1 ... pn) e):
 *   A function that when applied will evaluate e substituting ocurrences of
 *   the atoms pi with the corresponding arguments. Creates a closure over
 *   the current environment.
 * (define n x):
 *   Adds the name n to the global environment associated with value x.
 */
#define nil 0
#define EOS 1 /* this is used internally to signify end-of-string */
#define P_t -1
#define P_quote -4
#define P_atom -10
#define P_eq -15
#define P_car -18
#define P_cdr -22
#define P_cons -26
#define P_cond -31
#define P_lambda -36
#define P_define -43
#define NPRIMITIVES 10
L p_t(L args, L env);
L p_quote(L args, L env);
L p_atom(L args, L env);
L p_eq(L args, L env);
L p_car(L args, L env);
L p_cdr(L args, L env);
L p_cons(L args, L env);
L p_cond(L args, L env);
L p_lambda(L args, L env);
L p_define(L args, L env);
char *PRIMITIVE[10] = {
    "#t",  "quote", "atom", "eq",     "car",
    "cdr", "cons",  "cond", "lambda", "define",
};

/* Determines the type of a Lisp value. */
int T(L x) {
  if (x == nil)
    return T_nil;
  if (x > 0)
    return T_cell;
  return T_atom;
}

/* Cells.
 *
 * cons:
 *   Create a cell.
 * car:
 *   Access a cell's head.
 * cdr:
 *   Access a cell's tail.
 */
L cons(L head, L tail) {
  *cell_next++ = head;
  *cell_next++ = tail;
  return cell_next - CELL;
}
L car(L x) { return CELL[x - 2]; }
L cdr(L x) { return CELL[x - 1]; }

/* Atoms.
 *
 * atom:
 *   Creates a Lisp atom from a string of a given length.
 * atom_str:
 *   Returns the 0-terminated string for the Lisp atom.
 *
 * Helpers:
 *   atom_val: computes the Lisp value for a given pointer into ATOM.
 *   atom_find: searches ATOM for a string.
 */
L atom_val(char *a) { return -((a - ATOM) + 1); }
char *atom_find(char *a, int len) {
  char *c;
  char *x;
  int i;

  c = ATOM;
  while (c < atom_next) {
    x = c;
    for (i = 0; *c && i < len && a[i] == *c; ++i, ++c)
      ;
    if (i == len && *c == 0)
      return x;
    while (*c)
      ++c;
    ++c;
  }

  return 0;
}
char *atom_str(L x) { return &ATOM[-x - 1]; }
L atom(char *a, int len) {
  int i;
  char *x;

  if ((x = atom_find(a, len)))
    return atom_val(x);
  x = atom_next;
  for (i = 0; i < len; ++i)
    *atom_next++ = a[i];
  *atom_next++ = '\0';

  return atom_val(x);
}

/* Printing Lisp values.
 *
 * Here are the first functions where we can see what it's like to manipulate
 * our Lisp values. Since each Lisp value can only be nil, an atom, or a cell,
 * it's straightforward to handle each.
 *
 * nil -> ()
 * atom -> its string representation
 * cell -> space-separated recursive print
 */
void print_list(L list) {
  while (list != nil) {
    /* A cell can contain an atom, nil, or another cell. */
    if (T(list) == T_cell) {
      print(car(list));
      list = cdr(list);
      if (list != nil)
        putchar(SPACE);
    } else {
      print(list);
      break;
    }
  }
}
void print(L x) {
  char *s;
  if (x == EOS)
    return;
  if (T(x) == T_nil) {
    putchar(LPAREN);
    putchar(RPAREN);
  } else if (T(x) == T_atom) {
    for (s = atom_str(x); *s; ++s)
      putchar(*s);
  } else {
    putchar(LPAREN);
    print_list(x);
    putchar(RPAREN);
  }
}

/* Evaluation.
 *
 * Here's the core of our Lisp.
 *
 * eval:
 *   nil -> nil
 *   atom -> env_get
 *   cell -> apply
 * apply:
 *   primitive op -> apply_primitive
 *   lambda -> apply_lambda
 * apply_primitive:
 *   dispatch to a primitive function
 * apply_lambda:
 *   We evaluate the function arguments with the caller's env.
 *   We bind those evaluated arguments to the parameter names of the lambda in
 *   the lambda's captured environment.
 *   Finally, we evaluate the lambda expression under its captured environment.
 */
int isprimitive(L x) { return T(x) == T_atom && atom_str(x) < p_end; }
L apply_primitive(L op, L args, L env) {
  switch (op) {
  case P_t:
    return p_t(args, env);
  case P_quote:
    return p_quote(args, env);
  case P_atom:
    return p_atom(args, env);
  case P_eq:
    return p_eq(args, env);
  case P_car:
    return p_car(args, env);
  case P_cdr:
    return p_cdr(args, env);
  case P_cons:
    return p_cons(args, env);
  case P_cond:
    return p_cond(args, env);
  case P_lambda:
    return p_lambda(args, env);
  case P_define:
    return p_define(args, env);
  }
  return nil;
}
int eq(L x, L y) { return T(x) != T_cell && x == y; }
L env_get(L atom, L env) {
  /* env is structured as ((name val) (name val) ...) */
  while (env != nil && !eq(car(car(env)), atom))
    env = cdr(env);
  if (env != nil)
    return cdr(car(env));
  return nil;
}
L bind(L names, L vals, L tail) {
  return T(names) == T_nil    ? tail
         : T(names) == T_cell ? bind(cdr(names), cdr(vals),
                                     cons(cons(car(names), car(vals)), tail))
                              : cons(cons(names, vals), tail);
}
L eval_list(L exprs, L env) {
  return T(exprs) == T_atom ? env_get(exprs, env)
         : T(exprs) == T_cell
             ? cons(eval(car(exprs), env), eval_list(cdr(exprs), env))
             : nil;
}
L apply_lambda(L lambda, L args, L env) {
  L fnenv;
  args = eval_list(args, env);

  /* lambda is structured as: (((parameters) expr) env) */
  fnenv = cdr(lambda);
  if (fnenv == nil)
    fnenv = env;
  fnenv = bind(car(car(lambda)), args, fnenv);

  return eval(car(cdr(car(lambda))), fnenv);
}
L apply(L op, L args, L env) {
  return isprimitive(op) ? apply_primitive(op, args, env)
                         : apply_lambda(op, args, env);
}
L eval(L expr, L env) {
  if (expr == EOS)
    return EOS;
  env = env == 0 ? genv : env;
  return T(expr) == T_atom   ? env_get(expr, env)
         : T(expr) == T_cell ? apply(eval(car(expr), env), cdr(expr), env)
                             : nil;
}

/* Parsing.
 *
 * parse, parse_list, and parse_atom each parse a single item (an expression, a
 * list, and an atom, respectively) and advance the char** iterator past it.
 *
 * parse also handles whitespace and comments between expressions.
 */
L parse_atom(char **s) {
  char *a;
  int len;
  a = *s;
  len = 0;
  while (**s && **s != SPACE && **s != NEWLINE && **s != RPAREN) {
    ++*s;
    ++len;
  }
  return atom(a, len);
}
L parse_list(char **s) {
  L x;
  if (**s == RPAREN) {
    ++*s;
    return nil;
  }
  x = parse(s);
  return cons(x, parse_list(s));
}
L parse(char **s) {
  int clean;

  /* Ignore whitespace and comments */
  clean = 0;
  while (!clean) {
    clean = 1;
    while (**s == SPACE || **s == NEWLINE) {
      clean = 0;
      ++*s;
    }

    if (**s && **s == SEMICOLON) {
      clean = 0;
      while (**s && **s != NEWLINE)
        ++*s;
    }
  }

  if (!**s)
    return EOS;

  if (**s == LPAREN) {
    ++*s;
    return parse_list(s);
  } else {
    return parse_atom(s);
  }
}

/* Primitive implementations. */
L p_t(L args, L env) { return nil; }
L p_quote(L args, L env) { return car(args); }
L p_atom(L args, L env) {
  return T(eval(car(args), env)) == T_cell ? nil : P_t;
}
L p_eq(L args, L env) {
  args = eval_list(args, env);
  return eq(car(args), car(cdr(args))) ? P_t : nil;
}
L p_car(L args, L env) { return car(eval(car(args), env)); }
L p_cdr(L args, L env) { return cdr(eval(car(args), env)); }
L p_cons(L args, L env) {
  args = eval_list(args, env);
  return cons(car(args), car(cdr(args)));
}
L p_cond(L args, L env) {
  while (args != nil && eval(car(car(args)), env) == nil)
    args = cdr(args);
  if (args == nil)
    return nil;
  return eval(car(cdr(car(args))), env);
}
L p_lambda(L args, L env) { return cons(args, env); }
L p_define(L args, L env) {
  genv = bind(car(args), eval(car(cdr(args)), env), genv);
  return car(args);
}

/* To avoid the dependency, we have our own strlen here. */
long unsigned int strlen(const char *s) {
  int i;
  for (i = 0; *s; ++i, ++s)
    ;
  return i;
}

void init() {
  L x;
  int i;

  atom_next = ATOM;
  cell_next = CELL;
  genv = nil;

  /* Create atoms for the primitives and add them to the environment. */
  for (i = 0; i < NPRIMITIVES; ++i) {
    x = atom(PRIMITIVE[i], strlen(PRIMITIVE[i]));
    genv = bind(x, x, genv);
  }

  p_end = atom_next;
}
