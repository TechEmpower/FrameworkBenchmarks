#ifndef NEWTEST_H_
#define NEWTEST_H_
#include "string"
using namespace std;
public class NewTest{
NewTest(int a,string b);
private:
int a = 1;
string b;
protected:
public:
float c = 1.1;
public void setA(int a);
public string getstr(int e);
public string service(int a,string message);
}
#endif