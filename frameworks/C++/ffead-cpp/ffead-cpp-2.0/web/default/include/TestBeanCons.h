/*
	Copyright 2009-2012, Sumeet Chhetri 
  
    Licensed under the Apache License, Version 2.0 (the "License"); 
    you may not use this file except in compliance with the License. 
    You may obtain a copy of the License at 
  
        http://www.apache.org/licenses/LICENSE-2.0 
  
    Unless required by applicable law or agreed to in writing, software 
    distributed under the License is distributed on an "AS IS" BASIS, 
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
    See the License for the specific language governing permissions and 
    limitations under the License.  
*/

#ifndef TESTBEANCONS_H_
#define TESTBEANCONS_H_
#include "string"
#include "iostream"
#include "DependencyBean4.h"
using namespace std;
class TestBeanCons
{
private:
    int intProp;
    bool boolProp;
    DependencyBean4 dependencyBean4;
public:
    TestBeanCons()
    {
    }     
    TestBeanCons(int intProp,bool boolProp,DependencyBean4 dependencyBean4)
    {
        this->intProp = intProp;
        this->boolProp = boolProp;
        this->dependencyBean4 = dependencyBean4;
    }

    int getIntProp()
    {
        return this->intProp;
    }

    bool getBoolProp()
    {
        return this->boolProp;
    }

    DependencyBean4 getDependencyBean4()
    {
        return this->dependencyBean4;
    }

    void print()
    {
        if(this->boolProp)
        {
            this->dependencyBean4.print();
            cout << (this->intProp) << flush;
        }
    }
};
#endif /* TESTBEANCONS_H_ */
