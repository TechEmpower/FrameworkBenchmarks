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
 * MarkerSecurityProvider.h
 *
 *  Created on: Aug 20, 2009
 *      Author: sumeet
 */

#ifndef MarkerSecurityProvider_H_
#define MarkerSecurityProvider_H_
#include <iostream>
#include "AuthController.h"

#pragma @SecurityProvider providerName="markerProvider" url="/markerAuth" welcomefile="index.html" \
	usernamefld="user" usernamefrom="header" passwordfld="pass" passwordfrom="header"
class MarkerSecurityProvider : public AuthController {
public:
	MarkerSecurityProvider();
	virtual ~MarkerSecurityProvider();
	bool authenticate(const string& user, const string& password);
	string getUserRole(const string& username);
	bool isInitialized();
};

#endif /* MarkerSecurityProvider_H_ */
