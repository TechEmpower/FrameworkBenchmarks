/*
	Copyright 2009-2012, Sumeet Chhetri 
  
    Licensed under the Apache License, Version 2.0 (const the& "License"); 
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
 * Renderer.h
 *
 *  Created on: Jan 2, 2010
 *      Author: sumeet
 */

#ifndef RENDERER_H_
#define RENDERER_H_
#include "string"
#include "map"
using namespace std;

class Renderer {
public:
	Renderer();
	virtual ~Renderer();
	string generateStartOpenTag(const string&);
	string generateEndOpenTag();
	string generateCloseTag(const string&);
	typedef map<string,string> AttributeList;
	string generateAttributes(const AttributeList&);
	virtual string render()=0;
};

#endif /* RENDERER_H_ */
