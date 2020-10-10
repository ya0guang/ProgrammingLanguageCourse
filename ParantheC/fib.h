void (*pc)();

void *fibr__m__n, *fibr__m__k, *applyr__m__kr__m__k, *applyr__m__kr__m__v;

struct continuations;
typedef struct continuations continuations;
struct continuations {
  enum {
    _init_continuations,
    _subr2_continuations,
    _subr1_continuations
  } tag;
  union {
    struct { void *_jumpr__m__out; } _init;
    struct { void *_fibr__m__subr1; void *_k; } _subr2;
    struct { void *_n; void *_k; } _subr1;
  } u;
};

void *continuationsr_init(void *jumpr__m__out);
void *continuationsr_subr2(void *fibr__m__subr1, void *k);
void *continuationsr_subr1(void *n, void *k);

void fib();
void applyr__m__k();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

