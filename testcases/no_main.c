#include  <stdio.h>

//*******FAILING TESTCASE************
//no main function

int g1;
bool gb;

int first(int p, int q){
	return p;
}

bool fun(){
	return true;
}

int foo(int a, bool b, char c[]){

	a  = 0;
	b = false;

	g1 = g1 + 1;

	//return fun();
	//return first(1, b);
	return first(1,2);
}

