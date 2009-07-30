#include <stdio.h>
#include <math.h>

int main(void);
int main(void)
{
 int i;
 double x,y,z1,z2,d;

 d = 0.1;
 x = 0.0;
 for(i=0;i<50;i++){
   x += d;
   y  = exp(-x);
   z1 = (6 - 2*x)/(6 + 4*x + x*x);
   z2 = (6 - 4*x + x*x)/(6 + 2*x);
   printf("% 6.2f % 11.4e % 11.4e % 11.4e\n",
             x,y,z1,z2);
 }
 return 0;
}
