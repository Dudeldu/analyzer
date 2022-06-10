// PARAM: --enable ana.float.interval
#include <assert.h>
#include <float.h>
#include <limits.h>
#include <math.h>

int main()
{
    double x, a = 2., b = 3. + 1;
    float y, c = 2.f, d = 3.f + 1;

    assert(x == 2.);  // UNKNOWN!
    assert(y == 2.f); // UNKNOWN!

    assert(a == 2.); // SUCCESS!
    assert(a < 10.); // SUCCESS!
    assert(a > 10.); // FAIL!

    assert(c == 2.f); // SUCCESS!
    assert(c < 10.f); // SUCCESS!
    assert(c > 10.f); // FAIL!

    x = (a + b) / 2.;  // naive way of computing the middle
    y = (c + d) / 2.; // naive way of computing the middle

    assert(x == 3.);  // SUCCESS!
    assert(y == 3.f); // SUCCESS!

    assert(-97. == x - 100.);
    assert(-97.f == y - 100.f);
    return 0;
}
