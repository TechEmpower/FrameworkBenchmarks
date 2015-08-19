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
/*
 * TestBeanProp.h
 *
 *  Created on: Oct 17, 2010
 *      Author: root
 */

#ifndef TESTBEANPROP_H_
#define TESTBEANPROP_H_
#include "string"
#include "iostream"
#include "DependencyBean1.h"
#include "DependencyBean2.h"
#include "DependencyBean3.h"
using namespace std;
class TestBeanProp
{
private:
    string *strProp;
    DependencyBean1 *dependencyBean1;
    DependencyBean2 *dependencyBean2;
    DependencyBean3 *dependencyBean3;
public:
    void setStrProp(string *strProp)
    {
        this->strProp = strProp;
    }
    string* getStrProp()
    {
        return this->strProp;
    }

    void setDependencyBean1(DependencyBean1 *dependencyBean1)
    {
        this->dependencyBean1 = dependencyBean1;
    }
    DependencyBean1* getDependencyBean1()
    {
        return this->dependencyBean1;
    }

    void setDependencyBean2(DependencyBean2 *dependencyBean2)
    {
        this->dependencyBean2 = dependencyBean2;
    }
    DependencyBean2* getDependencyBean2()
    {
        return this->dependencyBean2;
    }

    void setDependencyBean3(DependencyBean3 *dependencyBean3)
    {
        this->dependencyBean3 = dependencyBean3;
    }
    DependencyBean3* getDependencyBean3()
    {
        return this->dependencyBean3;
    }

    void print()
    {
        cout << *(this->strProp) << flush;
        this->dependencyBean1->print();
        this->dependencyBean2->print();
        this->dependencyBean3->print();
    }
};

#endif /* TESTBEANPROP_H_ */
