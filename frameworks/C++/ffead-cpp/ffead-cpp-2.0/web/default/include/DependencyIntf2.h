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
 * DependencyIntf2.h
 *
 *  Created on: Oct 17, 2010
 *      Author: root
 */

#ifndef DEPENDENCYINTF2_H_
#define DEPENDENCYINTF2_H_

class DependencyIntf2
{
public:
    DependencyIntf2(){}
    virtual ~DependencyIntf2() {}
    virtual void print2()=0;
};
#endif /* DEPENDENCYINTF2_H_ */
