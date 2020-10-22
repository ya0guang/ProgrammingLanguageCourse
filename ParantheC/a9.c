#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "a9.h"

void *closurer_clos(void *body, void *env)
{
  closure *_data = (closure *)malloc(sizeof(closure));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _clos_closure;
  _data->u._clos._body = body;
  _data->u._clos._env = env;
  return (void *)_data;
}

void *environmentr_emptyr__m__env()
{
  environment *_data = (environment *)malloc(sizeof(environment));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _emptyr__m__env_environment;
  return (void *)_data;
}

void *environmentr_extendr__m__env(void *ar__ex__, void *envr__ex__)
{
  environment *_data = (environment *)malloc(sizeof(environment));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _extendr__m__env_environment;
  _data->u._extendr__m__env._ar__ex__ = ar__ex__;
  _data->u._extendr__m__env._envr__ex__ = envr__ex__;
  return (void *)_data;
}

void *continuationr_emptyr__m__k(void *jumpr__m__out)
{
  continuation *_data = (continuation *)malloc(sizeof(continuation));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _emptyr__m__k_continuation;
  _data->u._emptyr__m__k._jumpr__m__out = jumpr__m__out;
  return (void *)_data;
}

void *continuationr_appr__m__clos(void *randr__ex__, void *envr__ex__, void *kr__ex__)
{
  continuation *_data = (continuation *)malloc(sizeof(continuation));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _appr__m__clos_continuation;
  _data->u._appr__m__clos._randr__ex__ = randr__ex__;
  _data->u._appr__m__clos._envr__ex__ = envr__ex__;
  _data->u._appr__m__clos._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *continuationr_appr__m__arg(void *closr__ex__, void *kr__ex__)
{
  continuation *_data = (continuation *)malloc(sizeof(continuation));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _appr__m__arg_continuation;
  _data->u._appr__m__arg._closr__ex__ = closr__ex__;
  _data->u._appr__m__arg._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *continuationr_let(void *bodyr__ex__, void *envr__ex__, void *kr__ex__)
{
  continuation *_data = (continuation *)malloc(sizeof(continuation));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _let_continuation;
  _data->u._let._bodyr__ex__ = bodyr__ex__;
  _data->u._let._envr__ex__ = envr__ex__;
  _data->u._let._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *continuationr_throw(void *vr__m__expr__ex__, void *envr__ex__)
{
  continuation *_data = (continuation *)malloc(sizeof(continuation));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _throw_continuation;
  _data->u._throw._vr__m__expr__ex__ = vr__m__expr__ex__;
  _data->u._throw._envr__ex__ = envr__ex__;
  return (void *)_data;
}

void *continuationr_if(void *conseqr__ex__, void *altr__ex__, void *envr__ex__, void *kr__ex__)
{
  continuation *_data = (continuation *)malloc(sizeof(continuation));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _if_continuation;
  _data->u._if._conseqr__ex__ = conseqr__ex__;
  _data->u._if._altr__ex__ = altr__ex__;
  _data->u._if._envr__ex__ = envr__ex__;
  _data->u._if._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *continuationr_zero(void *kr__ex__)
{
  continuation *_data = (continuation *)malloc(sizeof(continuation));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _zero_continuation;
  _data->u._zero._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *continuationr_subr1(void *kr__ex__)
{
  continuation *_data = (continuation *)malloc(sizeof(continuation));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _subr1_continuation;
  _data->u._subr1._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *continuationr_multr1(void *xr2r__ex__, void *envr__ex__, void *kr__ex__)
{
  continuation *_data = (continuation *)malloc(sizeof(continuation));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _multr1_continuation;
  _data->u._multr1._xr2r__ex__ = xr2r__ex__;
  _data->u._multr1._envr__ex__ = envr__ex__;
  _data->u._multr1._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *continuationr_multr2(void *vr1r__ex__, void *kr__ex__)
{
  continuation *_data = (continuation *)malloc(sizeof(continuation));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _multr2_continuation;
  _data->u._multr2._vr1r__ex__ = vr1r__ex__;
  _data->u._multr2._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *exprr_const(void *cexp)
{
  expr *_data = (expr *)malloc(sizeof(expr));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _const_expr;
  _data->u._const._cexp = cexp;
  return (void *)_data;
}

void *exprr_var(void *n)
{
  expr *_data = (expr *)malloc(sizeof(expr));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _var_expr;
  _data->u._var._n = n;
  return (void *)_data;
}

void *exprr_if(void *test, void *conseq, void *alt)
{
  expr *_data = (expr *)malloc(sizeof(expr));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _if_expr;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *exprr_mult(void *nexpr1, void *nexpr2)
{
  expr *_data = (expr *)malloc(sizeof(expr));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _mult_expr;
  _data->u._mult._nexpr1 = nexpr1;
  _data->u._mult._nexpr2 = nexpr2;
  return (void *)_data;
}

void *exprr_subr1(void *nexp)
{
  expr *_data = (expr *)malloc(sizeof(expr));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _subr1_expr;
  _data->u._subr1._nexp = nexp;
  return (void *)_data;
}

void *exprr_zero(void *nexp)
{
  expr *_data = (expr *)malloc(sizeof(expr));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _zero_expr;
  _data->u._zero._nexp = nexp;
  return (void *)_data;
}

void *exprr_letcc(void *body)
{
  expr *_data = (expr *)malloc(sizeof(expr));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _letcc_expr;
  _data->u._letcc._body = body;
  return (void *)_data;
}

void *exprr_throw(void *kexp, void *vexp)
{
  expr *_data = (expr *)malloc(sizeof(expr));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _throw_expr;
  _data->u._throw._kexp = kexp;
  _data->u._throw._vexp = vexp;
  return (void *)_data;
}

void *exprr_let(void *exp, void *body)
{
  expr *_data = (expr *)malloc(sizeof(expr));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _let_expr;
  _data->u._let._exp = exp;
  _data->u._let._body = body;
  return (void *)_data;
}

void *exprr_lambda(void *body)
{
  expr *_data = (expr *)malloc(sizeof(expr));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _lambda_expr;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *exprr_app(void *rator, void *rand)
{
  expr *_data = (expr *)malloc(sizeof(expr));
  if (!_data)
  {
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  _data->tag = _app_expr;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

int main()
{
  exp = (void *)exprr_let(exprr_lambda(exprr_lambda(exprr_if(exprr_zero(exprr_var((void *)0)), exprr_const((void *)1), exprr_mult(exprr_var((void *)0), exprr_app(exprr_app(exprr_var((void *)1), exprr_var((void *)1)), exprr_subr1(exprr_var((void *)0))))))), exprr_mult(exprr_letcc(exprr_app(exprr_app(exprr_var((void *)1), exprr_var((void *)1)), exprr_throw(exprr_var((void *)0), exprr_app(exprr_app(exprr_var((void *)1), exprr_var((void *)1)), exprr_const((void *)4))))), exprr_const((void *)5)));
  env = (void *)environmentr_emptyr__m__env();
  pc = &valuer__m__ofr__m__cps;
  mount_tram();
  printf("Result is: %d\n", (int)v);
}

void applyr__m__closure()
{
  closure *_c = (closure *)clos;
  switch (_c->tag)
  {
  case _clos_closure:
  {
    void *body = _c->u._clos._body;
    void *envr__ex__ = _c->u._clos._env;
    env = (void *)environmentr_extendr__m__env(ar__m__ac, envr__ex__);
    exp = (void *)body;
    pc = &valuer__m__ofr__m__cps;
    break;
  }
  }
}

void applyr__m__env()
{
  environment *_c = (environment *)env;
  switch (_c->tag)
  {
  case _emptyr__m__env_environment:
  {
    fprintf(stderr, "unbound identifier");
    exit(1);
    break;
  }
  case _extendr__m__env_environment:
  {
    void *ar__ex__ = _c->u._extendr__m__env._ar__ex__;
    void *envr__ex__ = _c->u._extendr__m__env._envr__ex__;
    if ((yr__m__ae == 0))
    {
      v = (void *)ar__ex__;
      pc = &applyr__m__k;
    }
    else
    {
      env = (void *)envr__ex__;
      yr__m__ae = (void *)(void *)((int)yr__m__ae - 1);
      pc = &applyr__m__env;
    }
    break;
  }
  }
}

void applyr__m__k()
{
  continuation *_c = (continuation *)kr__m__cps;
  switch (_c->tag)
  {
  case _emptyr__m__k_continuation:
  {
    void *jumpr__m__out = _c->u._emptyr__m__k._jumpr__m__out;
    _trstr *trstr = (_trstr *)jumpr__m__out;
    longjmp(*trstr->jmpbuf, 1);
    break;
  }
  case _appr__m__clos_continuation:
  {
    void *randr__ex__ = _c->u._appr__m__clos._randr__ex__;
    void *envr__ex__ = _c->u._appr__m__clos._envr__ex__;
    void *kr__ex__ = _c->u._appr__m__clos._kr__ex__;
    kr__m__cps = (void *)continuationr_appr__m__arg(v, kr__ex__);
    exp = (void *)randr__ex__;
    env = (void *)envr__ex__;
    pc = &valuer__m__ofr__m__cps;
    break;
  }
  case _appr__m__arg_continuation:
  {
    void *closr__ex__ = _c->u._appr__m__arg._closr__ex__;
    void *kr__ex__ = _c->u._appr__m__arg._kr__ex__;
    clos = (void *)closr__ex__;
    ar__m__ac = (void *)v;
    kr__m__cps = (void *)kr__ex__;
    pc = &applyr__m__closure;
    break;
  }
  case _let_continuation:
  {
    void *bodyr__ex__ = _c->u._let._bodyr__ex__;
    void *envr__ex__ = _c->u._let._envr__ex__;
    void *kr__ex__ = _c->u._let._kr__ex__;
    env = (void *)environmentr_extendr__m__env(v, envr__ex__);
    exp = (void *)bodyr__ex__;
    kr__m__cps = (void *)kr__ex__;
    pc = &valuer__m__ofr__m__cps;
    break;
  }
  case _throw_continuation:
  {
    void *vr__m__expr__ex__ = _c->u._throw._vr__m__expr__ex__;
    void *envr__ex__ = _c->u._throw._envr__ex__;
    exp = (void *)vr__m__expr__ex__;
    env = (void *)envr__ex__;
    kr__m__cps = (void *)v;
    pc = &valuer__m__ofr__m__cps;
    break;
  }
  case _if_continuation:
  {
    void *conseqr__ex__ = _c->u._if._conseqr__ex__;
    void *altr__ex__ = _c->u._if._altr__ex__;
    void *envr__ex__ = _c->u._if._envr__ex__;
    void *kr__ex__ = _c->u._if._kr__ex__;
    if (v)
    {
      exp = (void *)conseqr__ex__;
      env = (void *)envr__ex__;
      kr__m__cps = (void *)kr__ex__;
      pc = &valuer__m__ofr__m__cps;
    }
    else
    {
      exp = (void *)altr__ex__;
      env = (void *)envr__ex__;
      kr__m__cps = (void *)kr__ex__;
      pc = &valuer__m__ofr__m__cps;
    }
    break;
  }
  case _zero_continuation:
  {
    void *kr__ex__ = _c->u._zero._kr__ex__;
    kr__m__cps = (void *)kr__ex__;
    v = (void *)(v == 0);
    pc = &applyr__m__k;
    break;
  }
  case _subr1_continuation:
  {
    void *kr__ex__ = _c->u._subr1._kr__ex__;
    v = (void *)(void *)((int)v - 1);
    kr__m__cps = (void *)kr__ex__;
    pc = &applyr__m__k;
    break;
  }
  case _multr1_continuation:
  {
    void *xr2r__ex__ = _c->u._multr1._xr2r__ex__;
    void *envr__ex__ = _c->u._multr1._envr__ex__;
    void *kr__ex__ = _c->u._multr1._kr__ex__;
    kr__m__cps = (void *)continuationr_multr2(v, kr__ex__);
    exp = (void *)xr2r__ex__;
    env = (void *)envr__ex__;
    pc = &valuer__m__ofr__m__cps;
    break;
  }
  case _multr2_continuation:
  {
    void *vr1r__ex__ = _c->u._multr2._vr1r__ex__;
    void *kr__ex__ = _c->u._multr2._kr__ex__;
    v = (void *)(void *)((int)vr1r__ex__ * (int)v);
    kr__m__cps = (void *)kr__ex__;
    pc = &applyr__m__k;
    break;
  }
  }
}

void valuer__m__ofr__m__cps()
{
  expr *_c = (expr *)exp;
  switch (_c->tag)
  {
  case _const_expr:
  {
    void *cexp = _c->u._const._cexp;
    v = (void *)cexp;
    pc = &applyr__m__k;
    break;
  }
  case _mult_expr:
  {
    void *xr1 = _c->u._mult._nexpr1;
    void *xr2 = _c->u._mult._nexpr2;
    kr__m__cps = (void *)continuationr_multr1(xr2, env, kr__m__cps);
    exp = (void *)xr1;
    pc = &valuer__m__ofr__m__cps;
    break;
  }
  case _subr1_expr:
  {
    void *x = _c->u._subr1._nexp;
    kr__m__cps = (void *)continuationr_subr1(kr__m__cps);
    exp = (void *)x;
    pc = &valuer__m__ofr__m__cps;
    break;
  }
  case _zero_expr:
  {
    void *x = _c->u._zero._nexp;
    kr__m__cps = (void *)continuationr_zero(kr__m__cps);
    exp = (void *)x;
    pc = &valuer__m__ofr__m__cps;
    break;
  }
  case _if_expr:
  {
    void *test = _c->u._if._test;
    void *conseq = _c->u._if._conseq;
    void *alt = _c->u._if._alt;
    kr__m__cps = (void *)continuationr_if(conseq, alt, env, kr__m__cps);
    exp = (void *)test;
    pc = &valuer__m__ofr__m__cps;
    break;
  }
  case _letcc_expr:
  {
    void *body = _c->u._letcc._body;
    env = (void *)environmentr_extendr__m__env(kr__m__cps, env);
    exp = (void *)body;
    pc = &valuer__m__ofr__m__cps;
    break;
  }
  case _throw_expr:
  {
    void *kr__m__exp = _c->u._throw._kexp;
    void *vr__m__exp = _c->u._throw._vexp;
    kr__m__cps = (void *)continuationr_throw(vr__m__exp, env);
    exp = (void *)kr__m__exp;
    pc = &valuer__m__ofr__m__cps;
    break;
  }
  case _let_expr:
  {
    void *e = _c->u._let._exp;
    void *body = _c->u._let._body;
    kr__m__cps = (void *)continuationr_let(body, env, kr__m__cps);
    exp = (void *)e;
    pc = &valuer__m__ofr__m__cps;
    break;
  }
  case _var_expr:
  {
    void *y = _c->u._var._n;
    yr__m__ae = (void *)y;
    pc = &applyr__m__env;
    break;
  }
  case _lambda_expr:
  {
    void *body = _c->u._lambda._body;
    v = (void *)closurer_clos(body, env);
    pc = &applyr__m__k;
    break;
  }
  case _app_expr:
  {
    void *rator = _c->u._app._rator;
    void *rand = _c->u._app._rand;
    kr__m__cps = (void *)continuationr_appr__m__clos(rand, env, kr__m__cps);
    exp = (void *)rator;
    pc = &valuer__m__ofr__m__cps;
    break;
  }
  }
}

int mount_tram()
{
  srand(time(NULL));
  jmp_buf jb;
  _trstr trstr;
  void *dismount;
  int _status = setjmp(jb);
  trstr.jmpbuf = &jb;
  dismount = &trstr;
  if (!_status)
  {
    kr__m__cps = (void *)continuationr_emptyr__m__k(dismount);
    for (;;)
    {
      pc();
    }
  }
  return 0;
}
