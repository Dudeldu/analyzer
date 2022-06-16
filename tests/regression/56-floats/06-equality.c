// PARAM: --enable ana.float.interval --enable warn.float

void main()
{
    int check1 = (0.2 == 0.2); // WARN
    int check2 = (0.2 != 0.3); // WARN

    // The following is __DBL_MAX__
    double my_max = 1.79769313486231570814527423731704357e+308L;

    // As numbers this big cannot be represented exactly anymore by double, this "- 1" will get
    // completely consumed.
    double max_minus_one = my_max - 1; // WARN
}
