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
 * DependencyIntf1.h
 *
 *  Created on: Oct 17, 2010
 *      Author: root
 */

#ifndef DEPENDENCYINTF1_H_
#define DEPENDENCYINTF1_H_

class DependencyIntf1
{
public:
    DependencyIntf1(){}
    virtual ~DependencyIntf1() {}
    virtual void print1()=0;
};
#endif /* DEPENDENCYINTF1_H_ */
