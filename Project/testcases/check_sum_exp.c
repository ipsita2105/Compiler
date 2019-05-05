#include  <stdio.h>

//**********FAILING TESTCASE*************

void main(){

	int x,y ;
	char c[5] ;
	char d;
	bool b;
	int line[100];
	int flag = 0;

	//this is allowed
	b = !b;

	//this will fail
	//b = !x;

	// can increment only int
	b ++;

	return ;
}
