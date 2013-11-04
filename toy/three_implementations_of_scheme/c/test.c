#include <stdio.h>
#include <stdlib.h>
int main()
{
	char * a = (char*)malloc(sizeof(char) * 2);
	a[0] = 'a';
	a[1] = 0;
	printf("%s", a);
	return 0;
}
