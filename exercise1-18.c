#include <stdio.h>

/* remove all trailing blanks and tabs and empty lines */ 

#define	maxline	1000	/* max length of array s */ 

int main() 

{
	int i = 0;
	int c = '\n'; 
	char s[maxline];

	while (c!=EOF){
		for (i = 0; (c=getchar())!=EOF && c!='\n'; ++i)
			s[i]=c; 
		
		for (--i; i >= 0 && (s[i]=='\t' || s[i]==' '); --i)
			;
		for (int j = 0; j <= i; ++j)
			printf("%c", s[j]);
		if (c == '\n' && i >= 0)
			printf(".\n"); 
	}
}
				
