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
 * DepDependencyBean2Impl.h
 *
 *  Created on: Oct 17, 2010
 *      Author: root
 */

#ifndef DEPDEPENDENCYBEAN2IMPL_H_
#define DEPDEPENDENCYBEAN2IMPL_H_

#include "DependencyIntf2.h"
#include "iostream"
class DepDependencyBean2Impl : public DependencyIntf2
{
public:
    void print2()
    {
        cout <<"World!!" << flush;
    }
};
#endif /* DEPDEPENDENCYBEAN2IMPL_H_ */
