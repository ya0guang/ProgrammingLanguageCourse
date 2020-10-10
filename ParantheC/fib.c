#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "fib.h"

void *continuationsr_init(void *jumpr__m__out) {
continuations* _data = (continuations*)malloc(sizeof(continuations));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _init_continuations;
  _data->u._init._jumpr__m__out = jumpr__m__out;
  return (void *)_data;
}

void *continuationsr_subr2(void *fibr__m__subr1, void *k) {
continuations* _data = (continuations*)malloc(sizeof(continuations));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr2_continuations;
  _data->u._subr2._fibr__m__subr1 = fibr__m__subr1;
  _data->u._subr2._k = k;
  return (void *)_data;
}

void *continuationsr_subr1(void *n, void *k) {
continuations* _data = (continuations*)malloc(sizeof(continuations));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_continuations;
  _data->u._subr1._n = n;
  _data->u._subr1._k = k;
  return (void *)_data;
}

int main()
{
fibr__m__n = (void *)(void *)5;
pc = &fib;
mount_tram();
printf("Fib of 5 is %d\n", (int)applyr__m__kr__m__v);}

void applyr__m__k()
{
continuations* _c = (continuations*)applyr__m__kr__m__k;
switch (_c->tag) {
case _init_continuations: {
void *jumpr__m__out = _c->u._init._jumpr__m__out;
_trstr *trstr = (_trstr *)jumpr__m__out;
longjmp(*trstr->jmpbuf, 1);
break; }
case _subr2_continuations: {
void *fibr__m__subr1 = _c->u._subr2._fibr__m__subr1;
void *k = _c->u._subr2._k;
applyr__m__kr__m__k = (void *)k;
applyr__m__kr__m__v = (void *)(void *)((int)fibr__m__subr1 + (int)applyr__m__kr__m__v);
pc = &applyr__m__k;
break; }
case _subr1_continuations: {
void *n = _c->u._subr1._n;
void *k = _c->u._subr1._k;
fibr__m__k = (void *)continuationsr_subr2(applyr__m__kr__m__v,k);
fibr__m__n = (void *)(void *)((int)(void *)((int)n - 1) - 1);
pc = &fib;
break; }
}
}

void fib()
{
if((fibr__m__n < (void *)2)) {
  applyr__m__kr__m__k = (void *)fibr__m__k;
applyr__m__kr__m__v = (void *)(void *)1;
pc = &applyr__m__k;

} else {
  fibr__m__k = (void *)continuationsr_subr1(fibr__m__n,fibr__m__k);
fibr__m__n = (void *)(void *)((int)fibr__m__n - 1);
pc = &fib;

}
}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
fibr__m__k= (void *)continuationsr_init(dismount);
for(;;) {
pc();
}
}
return 0;
}
