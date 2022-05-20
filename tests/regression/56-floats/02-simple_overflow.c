// PARAM: --enable ana.float.interval
#include <stdio.h>

void main()
{
    double data;
    // TODO: When we have this uncommented, the add is not performed?? Why!?
    // fscanf(stdin, "%lf", &data);
    
    double result = data + 1.0;  // WARN: potential overflow
    printf("%lf\n", result);
}
