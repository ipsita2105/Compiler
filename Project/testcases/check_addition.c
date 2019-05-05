#include  <stdio.h>

//**********FAILING TESTCASE*************

int foo(int a, bool b, char c[]){
        return a;
}

bool fun(){
        return true;
}

void main(){

	int x,y ;
	char d;
	bool b;
	int line[100];
	int flag = 0;

	// can't add bool to int
	//x = y + b;

	x = y/2;	

	// adding int together is correct	
	y = x + foo(x, b, c);

	//this will fail
        y = x + fun();

	return ;
}
