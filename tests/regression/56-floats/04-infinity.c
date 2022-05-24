#include <assert.h>
//#include <float.h>
//#include <limits.h>
//#include <math.h>

int main()
{
    double a = 2.;
    double b = 4.;

    // TODO: Does this work? In any case, right now we have, in theory, two warnings, one for infinity, and one for
    //       division by zero - but the latter doesn't actually make sense, as is seen here ;-)
    //       We need to remove it.
    double x = 4. / 0.; // Should give a warning that we have infinity here

    return 0;
}
