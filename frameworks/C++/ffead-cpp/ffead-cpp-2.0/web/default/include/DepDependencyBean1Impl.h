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
 * DepDependencyBean1Impl.h
 *
 *  Created on: Oct 17, 2010
 *      Author: root
 */

#ifndef DEPDEPENDENCYBEAN1IMPL_H_
#define DEPDEPENDENCYBEAN1IMPL_H_
#include "DependencyIntf1.h"
#include "iostream"
class DepDependencyBean1Impl : public DependencyIntf1
{
public:
    void print1()
    {
        cout <<"Hello " << flush;
    }
};
#endif /* DEPDEPENDENCYBEAN1IMPL_H_ */
