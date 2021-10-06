#include <assert.h>
#include <pthread.h>

int g = 1;

void foo() {
    g = 2;
}

void* t_fun(void *arg) {
    foo();
    return NULL;
}

void* t_fun2(void *arg) {
    assert(g == 1); // unknown before, success after
    return NULL;
}

int main() {
    pthread_t id, id2;
    pthread_create(&id2, NULL, t_fun2, NULL);
    pthread_create(&id, NULL, t_fun, NULL);
    return 0;
}