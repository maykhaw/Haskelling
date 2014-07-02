#include <stdio.h> 

#define	IN	1	/* inside a word */ 
#define	OUT	0	/* outside a word */ 

/* print histogram of word counts from input file */ 

int main() 

{

	int c, state, nw; 

	{state = OUT; 
	nw = 0; 

	while ((c = getchar()) != EOF) {
		if (c == ' ' || c == '\n' || c == '\t' )
			state = OUT; 
		else if (state == OUT) {
			state = IN; 
			++nw;
		}
	}
	}

	int nword[nw];
	
	state = IN; 
	nword = 0; 
	while ((c = getchar()) != EOF) {
		if (c == ' ' || c == '\n' || c == '\t' ) 
			state = OUT; 
		else if (state == OUT) {
			state = IN;
			++nword;
		}
	}
} 

	
	
	printf("%d\n", nw); 
} 

