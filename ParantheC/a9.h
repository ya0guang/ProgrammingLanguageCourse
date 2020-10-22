void *exp, *kr__m__cps, *v, *clos, *ar__m__ac, *env, *yr__m__ae;

void (*pc)();

struct expr;
typedef struct expr expr;
struct expr {
  enum {
    _const_expr,
    _var_expr,
    _if_expr,
    _mult_expr,
    _subr1_expr,
    _zero_expr,
    _letcc_expr,
    _throw_expr,
    _let_expr,
    _lambda_expr,
    _app_expr
  } tag;
  union {
    struct { void *_cexp; } _const;
    struct { void *_n; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_nexpr1; void *_nexpr2; } _mult;
    struct { void *_nexp; } _subr1;
    struct { void *_nexp; } _zero;
    struct { void *_body; } _letcc;
    struct { void *_kexp; void *_vexp; } _throw;
    struct { void *_exp; void *_body; } _let;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *exprr_const(void *cexp);
void *exprr_var(void *n);
void *exprr_if(void *test, void *conseq, void *alt);
void *exprr_mult(void *nexpr1, void *nexpr2);
void *exprr_subr1(void *nexp);
void *exprr_zero(void *nexp);
void *exprr_letcc(void *body);
void *exprr_throw(void *kexp, void *vexp);
void *exprr_let(void *exp, void *body);
void *exprr_lambda(void *body);
void *exprr_app(void *rator, void *rand);

void valuer__m__ofr__m__cps();
struct continuation;
typedef struct continuation continuation;
struct continuation {
  enum {
    _emptyr__m__k_continuation,
    _appr__m__clos_continuation,
    _appr__m__arg_continuation,
    _let_continuation,
    _throw_continuation,
    _if_continuation,
    _zero_continuation,
    _subr1_continuation,
    _multr1_continuation,
    _multr2_continuation
  } tag;
  union {
    struct { void *_jumpr__m__out; } _emptyr__m__k;
    struct { void *_randr__ex__; void *_envr__ex__; void *_kr__ex__; } _appr__m__clos;
    struct { void *_closr__ex__; void *_kr__ex__; } _appr__m__arg;
    struct { void *_bodyr__ex__; void *_envr__ex__; void *_kr__ex__; } _let;
    struct { void *_vr__m__expr__ex__; void *_envr__ex__; } _throw;
    struct { void *_conseqr__ex__; void *_altr__ex__; void *_envr__ex__; void *_kr__ex__; } _if;
    struct { void *_kr__ex__; } _zero;
    struct { void *_kr__ex__; } _subr1;
    struct { void *_xr2r__ex__; void *_envr__ex__; void *_kr__ex__; } _multr1;
    struct { void *_vr1r__ex__; void *_kr__ex__; } _multr2;
  } u;
};

void *continuationr_emptyr__m__k(void *jumpr__m__out);
void *continuationr_appr__m__clos(void *randr__ex__, void *envr__ex__, void *kr__ex__);
void *continuationr_appr__m__arg(void *closr__ex__, void *kr__ex__);
void *continuationr_let(void *bodyr__ex__, void *envr__ex__, void *kr__ex__);
void *continuationr_throw(void *vr__m__expr__ex__, void *envr__ex__);
void *continuationr_if(void *conseqr__ex__, void *altr__ex__, void *envr__ex__, void *kr__ex__);
void *continuationr_zero(void *kr__ex__);
void *continuationr_subr1(void *kr__ex__);
void *continuationr_multr1(void *xr2r__ex__, void *envr__ex__, void *kr__ex__);
void *continuationr_multr2(void *vr1r__ex__, void *kr__ex__);

void applyr__m__k();
struct environment;
typedef struct environment environment;
struct environment {
  enum {
    _emptyr__m__env_environment,
    _extendr__m__env_environment
  } tag;
  union {
    struct { char dummy; } _emptyr__m__env;
    struct { void *_ar__ex__; void *_envr__ex__; } _extendr__m__env;
  } u;
};

void *environmentr_emptyr__m__env();
void *environmentr_extendr__m__env(void *ar__ex__, void *envr__ex__);

void applyr__m__env();
struct closure;
typedef struct closure closure;
struct closure {
  enum {
    _clos_closure
  } tag;
  union {
    struct { void *_body; void *_env; } _clos;
  } u;
};

void *closurer_clos(void *body, void *env);

void applyr__m__closure();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

