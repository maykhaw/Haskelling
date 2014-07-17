#include <stdio.h>

#define	maxline	1000	/* max length of array */ 

char reverse(s[maxline])
{
	int i = 0;
	int c = '\n'; 
	char s[maxline];

	while (c!=EOF) { 
		for (i = 0; (c=getchar())!=EOF && c!='\n' && i < maxline; ++i) 
			s[i] = c; 
		if (i >= maxline)
			return 1; 
		for (; i >= 0; --i) 
			printf("%c", s[i]); 
		printf("\n");
	}
}
