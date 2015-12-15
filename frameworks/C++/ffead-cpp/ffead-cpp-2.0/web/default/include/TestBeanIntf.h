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
  TestBeanIntf.h

   Created on: Oct 17, 2010
       Author: root
 */

#ifndef TESTBEANINTF_H_
#define TESTBEANINTF_H_

#include "DependencyIntf1.h"
#include "DependencyIntf2.h"
class TestBeanIntf
{
private:
    DependencyIntf1 *dependencyIntf1;
    DependencyIntf2 *dependencyIntf2;
public:
    void setDependencyIntf1(DependencyIntf1 *dependencyIntf1)
    {
        this->dependencyIntf1 = dependencyIntf1;
    }

    void setDependencyIntf2(DependencyIntf2 *dependencyIntf2)
    {
        this->dependencyIntf2 = dependencyIntf2;
    }

    void print()
    {
        this->dependencyIntf1->print1();
        this->dependencyIntf2->print2();
    }
};
#endif /* TESTBEANINTF_H_ */
