// PARAM: --enable ana.float.interval
#include <stdio.h>

void main()
{
    double data;
    fscanf(stdin, "%lf", &data);
    
    int check = (data == 0.3); // WARN

    
    double x = 0.1;
    double y = 0.2;

    int check2 = (x == y); // WARN
}
