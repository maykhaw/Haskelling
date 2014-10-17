#include <stdio.h> 

/* Print array of words of i length */

int main()
{
	int c, nc, i;
	int ndigit[26]; 
	
	nc = 0; 
	for (i = 0; i < 26; ++i)
		ndigit[i] = 0; 

	while ((c = getchar()) != EOF) {
		int isSpace = (c == ' ' || c == '\n' || c == '\t');
		// printf("isSpace; %d\n", isSpace); 
		if ((nc > 0) && isSpace) {
			if ((nc >= 0) && (nc <= 25)){
				++ndigit[nc];
				// printf("nc; %d\n", nc);
			}
			nc = 0;
		}
		if (!isSpace) ++nc; 
	}
	if (nc > 0) {
		++ndigit[nc];
	}
	printf("word length popularity ="); 
	for (i =  25; (i >= 0) && (ndigit[i] == 0); --i)
		;
	int lastNonZero = i;
	for (i = 1; i <= lastNonZero; ++i) 
		printf(" %d", ndigit[i]);
	printf("\n");
}


