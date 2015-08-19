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
 * TimeUnit.h
 *
 *  Created on: Mar 23, 2010
 *      Author: sumeet
 */

#ifndef TIMEUNIT_H_
#define TIMEUNIT_H_
class TimeUnit {
public:
	static const int NANOSECONDS = 0;
	static const int MICROSECONDS = 1;
	static const int MILLISECONDS = 2;
	static const int SECONDS = 3;
	static const int MINUTES = 4;
	static const int HOURS = 5;
	static const int DAYS = 6;
};

#endif /* TIMEUNIT_H_ */
