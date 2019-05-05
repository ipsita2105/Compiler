#include <stdio.h>

int main()
{
    int n, i;
    int factorial = 1;

    // find 5 factorial
    n = 5;

    if (n < 0)
        print("Error! Factorial of a negative number doesn't exist.");

    else
    {
	i = 1;
        while(i<=n)
        {
            factorial *= i;              // factorial = factorial*i;
	    i++;
        }

        print("Factorial of ", n, "=",factorial);
    }

    return 0;
}
