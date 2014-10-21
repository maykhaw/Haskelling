#include <stdio.h>

#define	maxline	1000	/* max length of array */ 

<<<<<<< HEAD
char reverse(s[maxline])
=======
void reverse()

>>>>>>> 3a8cf7495bafede4676416ab6a243f30079f0486
{
	int i = 0;
	int c = '\n'; 
	char s[maxline];

<<<<<<< HEAD
	while (c!=EOF) { 
		for (i = 0; (c=getchar())!=EOF && c!='\n' && i < maxline; ++i) 
			s[i] = c; 
		if (i >= maxline)
			return 1; 
		for (; i >= 0; --i) 
=======
	while (c!=EOF){  
		for (i = 0; (c=getchar())!=EOF && c!='\n' && i < maxline; ++i){ 
			s[i] = c;	
		}
		if (i >= maxline) {
			printf ("Long lines not handled, yet.  Sorry!\n");
			return;
		}
		for (--i; i >= 0; --i) 
>>>>>>> 3a8cf7495bafede4676416ab6a243f30079f0486
			printf("%c", s[i]); 
		printf("\n");
	}
}
<<<<<<< HEAD
=======

int main() {
	reverse ();

}
>>>>>>> 3a8cf7495bafede4676416ab6a243f30079f0486
