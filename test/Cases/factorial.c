#include <stdio.h>
// body of IMP program
int main() {
   int n  = 0 ;
   scanf("%d",&n);
   int result  = 1 ;
   while ( 0 < n ) {
      result = result * n ;
      n = n - 1 ;
   }
   printf("%d",result);
   return 0;
}
