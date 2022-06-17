// PARAM: --enable ana.float.interval --enable warn.float
#include <float.h>

void main()
{
    int check1 = (0.2 == 0.2); // WARN
    int check2 = (0.2 != 0.3); // WARN

    // Not all integers that are this big are representable in doubles anymore...
    double high_value = 179769313486231568384.0;
    double x = high_value + 1; // WARN

    double a = DBL_MAX; // NOWARN
    double b = a - 1;   // WARN

    double e = (double)(1.7976931348623157e+308L); // NOWARN
    double f = e - 1;                              // WARN
}
