#include <stdio.h>

/* Write Fahrenheit - Celsius Conversion table using a function */ 

float Celsius(float fahr)

{

	return ((5.0 / 9.0) * (fahr - 32.0));
}

int main() 

{
	int lower = 0, upper = 300, step = 20; 
	float fahr; 

	fahr = lower; 
	while (fahr <= upper){
		printf("%3.0f %6.1f\n", fahr, Celsius(fahr)); 
		fahr = fahr + step;
	}

} 


	

