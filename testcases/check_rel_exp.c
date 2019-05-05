#include  <stdio.h>

//**********FAILING TESTCASE*************

void main(){

	int x,y ;
	char c[5] ;
	char d;
	bool b;
	int line[100];
	int flag = 0;
	
	b = true || false;
        b = (x == y);
        b = true && true;
        b = !b;
        b = 1 > x ;

	//this will fail
	x = 1 > y;

	return ;
}
