#include <stdio.h>

#define	maxalpha	256	/* max alphabet */
#define maxdigit	10	/* max digit */

int main() 

{
	int nl, ns, nt, nother;
	int c, i; 
	int ndigit[maxdigit], nalpha[maxalpha]; 

	nl = ns = nt = nother = 0; 
	for (i = 0; i < maxalpha; ++i)
		ndigit[i] = 0; 
	for (i = 0; i < maxalpha; ++i) 
		nalpha[i] = 0; 

	while ((c = getchar()) != EOF){
		if (c == '\n')
			++nl; 
		if (c == ' ')
			++ns; 
		if (c == '\t')
			++nt; 
		if (c >= '0' && c <= '9')
			++ndigit[i];
		else if ((c >= 'a' && c <= 'z') ||  (c >= 'A' && c <= 'Z'))
			++nalpha[i];   
		else ++nother; 
	}

	printf("Character Histogram\n");
	printf("NL %d\tNS %d\tNT %d\tNOTHER %d", nl, ns, nt, nother);
	for (i = 0; i < maxalpha; ++i) 
		if (ndigit[i] > 0)
			printf("%c\t%d", i, ndigit[i]);
	for (i = 0; i < maxalpha; ++i) 
		if (nalpha[i] > 0)
			printf("%c\t%d", i, nalpha[i]);

}  
	
