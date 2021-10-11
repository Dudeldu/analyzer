// SKIP PARAM: --set ana.activated[+] apron
#include <pthread.h>
#include <assert.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_benign(void *arg) {
  return NULL;
}

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  g = 12;
  h = 14;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int t;

  // Force multi-threaded handling
  pthread_t id2;
  pthread_create(&id2, NULL, t_benign, NULL);

  pthread_mutex_lock(&A);
  g = 12;
  h = 14;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  assert(g == h); //UNKNOWN
  // (FAIL), because initial value overwritten
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  g = t;
  h = t;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  assert(g == h);
  pthread_mutex_unlock(&A);

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  assert(g == h); //UNKNOWN!
  pthread_mutex_unlock(&A);

  return 0;
}
