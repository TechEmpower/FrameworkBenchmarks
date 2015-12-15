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
 * Tempo.h
 *
 *  Created on: Jan 22, 2010
 *      Author: sumeet
 */

#ifndef TEMPO_H_
#define TEMPO_H_
#include "iostream"
using namespace std;
class Tempo {
	void prin();
	int p;
	string h;
public:
	Tempo();
	virtual ~Tempo();
	void prin1();
	template<class T> void func()
	{
		cout << p << flush;
	}
};

#endif /* TEMPO_H_ */
