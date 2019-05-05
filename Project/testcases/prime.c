#include <stdio.h>

int main()
{
    int n,i = 3, count, c;

    char chh;
    chh = "yo";
    n = 10;

    if (n >= 1){
	print(2);
    	count = 2;
    }

    while (count <= n){
    
    	c = 2;

	while (c < i){
		
		if(i%c == 0){
			break;
		}
			
	   c++;
	}

	if(c == i){
		count++;
		print(i);
	}

	i++;
	
    }

    return 0;
}
