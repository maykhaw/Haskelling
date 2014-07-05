#include <stdio.h> 

/* Print number of characters for each word */ 

int main() 
{ 
	int c, nc, nw; 
 
	nc = nw = 0; 
	while ((c = getchar()) != EOF) {
		int isSpace = (c == ' ' || c == '\n' || c == '\t');
		if ((nc > 0) && isSpace) {
			printf("%d ", nc);
			nc = 0;
			++nw;
		}
		if (!isSpace) ++nc;
	}
	if (nc > 0) {
		printf("%d", nc);
		++nw;
	}
	printf("\n");
	printf("%d\n", nw); 
}

