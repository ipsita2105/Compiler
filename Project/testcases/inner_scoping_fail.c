#include  <stdio.h>

//*******FAILING TESTCASE***********

void main(){

	//d is defined as char here
	char d;
	//d used as char here
        d = "a";	
	
	if (true&&false) {

		int temp = 2;	

	}else{

		//d defined as bool here
		bool d;

		//d used as bool here
		d = true;		
	}
	
	//d again used as bool here
	//ERROR: should be char
	d = true;
	
	return ;
}
