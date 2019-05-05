#include  <stdio.h>

//******PASSING TESTCASE**********
//correct return type of each function

int g1;
bool gb;

int first(int p, int q){

	//return type is int
	return p;
}

bool fun(){

	//return type is bool
	return true;
}

int foo(int a, bool b, char c[]){

	a  = 0;
	b = false;

	g1 = g1 + 1;

	//return type of returned function is int
	return first(1,2);
}

void main(){

	int flag;
	char c[5] ;
	
	while(flag < 10){
		flag++;
	}
	
	gb = true;	
	
	return ;
}
