#include  <stdio.h>

//*************PASSING TESTCASE*****************

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

void main(){

	int x,y ;
	char c[5] ;
	char d;
	bool b;
	int line[100];
	int flag = 0;

        d = "a";	
	//d = 9;
	x ++;
	//d++;
	b = true || false;
	b = (x == y);
	b = true && true;
	b = !b;
	b = 1 > x ;
	x = x + 2;
	x = y/2;
	//new[5] = 1;
	
	y = x + foo(x, b, c);
	//y = x + fun();

	//if (line) {
	if (true&&false) {
		int temp = 2;	
		x += temp;		
	}else{
		bool d;
		d = true;		
		x = y;
	}
	
	//d = false;
	d = "k";

	//temp = 3;
	while(flag < 10){
		flag++;
	}
	
	gb = true;	
	
	return ;
}
