#include <stdio.h> 

/* Print number of characters for each word */ 

#define	IN	1	/* inside a word */ 
#define	OUT	0	/* outside a word */ 

int main() 

{ 
	int c, nc, state; 
 
	nc = 0; 
	state = IN;
	while ((c = getchar()) != EOF) {
		if (c == ' ' || c == '\n' || c == '\t')
			state = OUT; 
		if (state == IN)  
			++nc;
	printf("%d", nc);
	}
}

