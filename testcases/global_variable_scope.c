#include  <stdio.h>

//*****PASSING TEST***********

int g1;
bool gb;

bool fun(){

	//can use global variable here
	return gb;
}

void main(){

	int x,y ;
	char c[5] ;
	char d;
	bool b;
	int line[100];
	int flag = 0;

	//can use global variable here
	g1 = 0;
	
	while(flag < 10){
		flag++;
	}
	
	gb = true;	
	
	return ;
}
