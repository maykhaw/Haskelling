#include <stdio.h>

/* print all input lines longer than longline=80 characters */ 

#define	longline	80	/* longline is the number at which all lines are to be printed out */ 


int main()
{
	int i = 0;
	int c = '\n';
	char s[longline]; 
	
	while (c!=EOF){

		for (i=0; (c=getchar())!=EOF && c!='\n' && i < longline; ++i)
			s[i]=c; 
		
		if (i >= longline){
			for (i = 0; i < longline; ++i)
				printf("%c", s[i]);
			while ((c=getchar())!=EOF && c!='\n')
				printf("%c", c);
		}
	}
}	
