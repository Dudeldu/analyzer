#include <pthread.h>
#include <assert.h>

struct __anonstruct_PQUEUE_63 {
   int occupied ;
   pthread_mutex_t mtx ;
};
typedef struct __anonstruct_PQUEUE_63 PQUEUE;

PQUEUE pqb  ;

int pqueue_init(PQUEUE *qp)
{
  qp->occupied = 0;
  pthread_mutex_init(& qp->mtx, NULL);
  return (0);
}

int pqueue_put(PQUEUE *qp)
{
  pthread_mutex_lock(& qp->mtx);
  (qp->occupied) ++; // overflow gives < 0 values!
  pthread_mutex_unlock(& qp->mtx);
  return (1);
}

int pqueue_get(PQUEUE *qp)
{
  int got = 0;
  pthread_mutex_lock(& qp->mtx);
  while (qp->occupied <= 0) {
    // qp->occupied should not be just 0, unsoundness in old
    assert(qp->occupied == 0); // UNKNOWN (no interval, with overflow)
    // this assert should not refine!
  }
  // qp->occupied should not be Error int, unsoundness in global
  assert(qp->occupied != 0);
  if (qp->occupied > 0) {
    (qp->occupied) --;
    got = 1;
    pthread_mutex_unlock(& qp->mtx);
  } else {
    pthread_mutex_unlock(& qp->mtx);
  }
  return (got);
}

void *worker(void *arg )
{
  while (1) {
    pqueue_get(& pqb);
  }
  return NULL;
}

int main(int argc , char **argv )
{
  pthread_t tid;

  PQUEUE *qp = &pqb;
  pqueue_init(& pqb);
  pthread_create(& tid, NULL, & worker, NULL);

  for (int i = 1; i < argc; i++) {
    pqueue_put(& pqb);
  }
  return 0;
}
