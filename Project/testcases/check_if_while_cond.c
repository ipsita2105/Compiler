#include  <stdio.h>

//*********FAILING TESTCASE*************

void main(){

	int x,y ;
	char d;
	bool b;
	int line[100];
	
	//bool type condition
	if (true&&false) {
		int temp = 2;	
		x += temp;
	}

	//bool type condtion
	while (x <= y){
		x += 1;
	}	
	
	//bool type condition
	if(b){
		y = 1;

	}

	//this will fail
	if(line){
		y = 1;

	}

	
	return ;
}
