#include  <stdio.h>

//************FAILING TESTCASE**************

int g1;
bool gb;


int foo(int a, bool b, char c[]){

	g1 = g1 + 1;

	return 1;
}

void main(){

	int x,y ;
	char c[5] ;
	char d;
	bool b;

	//wrong parameter passed
	y = x + foo(x, b, d);
	
	return ;
}
