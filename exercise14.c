#include <stdio.h>

#define	maxalpha	256	/* max alphabet */
#define maxdigit	10	/* max digit */

int main() 

{
	int nl, ns, nt, nother;
	int i; 
	int c;
	int ndigit[maxdigit], nalpha[maxalpha]; 

	nl = 0; ns = 0; nt = 0; nother = 0; 
	for (i = 0; i < maxdigit; ++i)
		ndigit[i] = 0; 
	for (i = 0; i < maxalpha; ++i) 
		nalpha[i] = 0; 

	while ((c = getchar()) != EOF){
		// printf("!");
		// if (c < 0)
			// printf ("Smaller than zero!\n");
		if (c == '\n')
			++nl; 
		else if (c == ' ')
			++ns; 
		else if (c == '\t')
			++nt; 
		else if ((c >= '0') && (c <= '9'))
			++ndigit[c-'0'];
		else if (((c >= 'a') && (c <= 'z')) ||  ((c >= 'A') && (c <= 'Z')))
			++nalpha[c];   
		else ++nother; 
	}

	printf("Character Histogram\n");
	if (nl > 0){
		printf("NL\t");
		for (int currentstars = 0; currentstars < nl; ++currentstars)
			printf("*");
		printf("\n");
	}	
	if (ns > 0){
		printf("NS\t");
		for (int currentstars = 0; currentstars < ns; ++currentstars)
			printf("*");
		printf("\n");
	}
	if (nt > 0){
		printf("NT\t");
		for (int currentstars = 0; currentstars < nt; ++currentstars)
			printf("*");
		printf("\n");
	}
	if (nother > 0){
		printf("NOTHER\t");
		for (int currentstars = 0; currentstars < nother; ++currentstars)
			printf("*");
		printf("\n");
	}
	for (i = 0; i < maxdigit; ++i)
		if (ndigit[i] > 0){
			printf("%d\t", i);
			for (int currentstars = 0; currentstars < ndigit[i]; ++currentstars)
				printf("*");
			printf("\n");
		}
	for (i = 0; i < maxalpha; ++i)
		if (nalpha[i] > 0){
			printf("%c\t", i);
			for (int currentstars = 0; currentstars < nalpha[i]; ++currentstars)
				printf("*"); 
			printf("\n");
		}
}  
	
