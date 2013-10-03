//#define __debug_obj123


#include <iostream>
#include <functional>


using namespace std;
#include "rgc.H"
using namespace RGC;
struct c1: public Object
{
	Ref<Object> a;
	//Object* a;
};

void func1()
{
	cout << "func1 called" << endl;
}
void call10times(function<void()> func)
{
	for(int i=0;i<10000000;i++)
		func();
}
void aaaaa()
{
	Ref<Object> tmp=newObj<Object>();
	cout << tmp->refCount << endl;
	
	
	
	c1 obj;
	obj.a=tmp;
	cout << tmp->refCount << endl;
	obj.a=newObj<Object>();
	cout << tmp->refCount << endl;
	Ref<Object> r(obj.a);
	Ref<Object> r1(obj);
	obj.a=NULL;
	
	//tmp=new Object();
}







int rgctest_main(int argc, char **argv)
{
	//aaaaa();
	/*Ref<FileStream> fs("/dev/urandom",O_RDONLY);
	Ref<StreamReader> sr(*fs);
	Ref<StringBuilder> sb;
	cout << fs() << endl;
	sr->ReadLine(*sb);
	cout << sb->ToCString() << endl;*/
	aaaaa();
}
