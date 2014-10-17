#include <stdio.h>

#define	maxline	1000	/* max length of array */ 

void reverse()

{
	int i = 0;
	int c = '\n'; 
	char s[maxline];

	while (c!=EOF){  
		for (i = 0; (c=getchar())!=EOF && c!='\n' && i < maxline; ++i){ 
			s[i] = c;	
		}
		if (i >= maxline) {
			printf ("Long lines not handled, yet.  Sorry!\n");
			return;
		}
		for (--i; i >= 0; --i) 
			printf("%c", s[i]); 
		printf("\n");
	}
}

int main() {
	reverse ();

}
