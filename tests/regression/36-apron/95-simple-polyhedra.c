// SKIP PARAM: --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
// Example from https://www-apr.lip6.fr/~mine/publi/article-mine-HOSC06.pdf, adapted
void main(void) {
  int X = 0;
  int N = rand();
  if(N < 0 || N > 10000) { N = 0; }

  X = 2 * N;

  assert(X - 2 * N == 0);
  assert(X == 2 * N);

  if(X == 2 * N) {
    N = 8;
  } else {
    N = 42; //DEAD
  }

}
