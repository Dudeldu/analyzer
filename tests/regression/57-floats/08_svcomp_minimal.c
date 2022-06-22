#include <assert.h>

typedef union
{
    float value;
    unsigned int word;
} A;

int main()
{
    A a;
    a.word = 3212836864;
    float b = a.value;
    assert(b == -1.0f);

    return 0;
}
