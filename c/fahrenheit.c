
#include <stdio.h>
/* print Fahrenheit-Celsius Table for Fahr = 0, 20, ..., 300 */ 
int main()
{
	printf("Fahrenheit to Celsius Conversion Table\n");
	int fahr, celsius; 
	int lower, upper, step;

	lower = 0; 	/* lower limit of temperaturescale */ 
	upper = 300; 	/* upper limit */ 
	step = 20; 	/* step size */ 

	fahr = lower; 
	while (fahr <= upper) {
		celsius = 5 * (fahr-32) / 9;
		printf("%d\t%d\n", fahr, celsius);
		fahr = fahr + step; 
	}
}


//		celsius = 5 * (fahr-32) / 9;
//		
