// PARAM: --enable ana.float.interval
#include <stdio.h>

void main()
{
    double data;
    
    int check = (data == 0.3); // WARN

    
    double x = 0.1;
    double y = 0.2;

    int check2 = (x == y); // WARN

    // The following is __DBL_MAX__
    double my_max = 179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.000000;

    // As numbers this big cannot be represented exactly anymore by double, this "- 1" will get
    // completely consumed.
    double max_minus_one = my_max - 1; // WARN
}
