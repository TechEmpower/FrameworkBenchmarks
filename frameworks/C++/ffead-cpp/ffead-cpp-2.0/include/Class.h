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
 * Class.h
 *
 *  Created on: Nov 20, 2009
 *      Author: sumeet
 */

#ifndef CLASS_H_
#define CLASS_H_
#include "PropFileReader.h"
#include "Method.h"

typedef map<string,Method> Meth;
typedef map<string,Field> Fld;

class Class {
	string name;
	string access;
	string type;
	string package;
	string base;
	string interfaces;
	string lang;
	Meth methods;
	Fld fields;
	void addMethod(const Method&);
	void addField(const Field&);
public:
	Class();
	virtual ~Class();
	void generatee(const string&);
    string getName() const;
    void setName(const string&);
    string getAccess() const;
    void setAccess(const string&);
    string getType() const;
    void setType(const string&);
    string getPackage() const;
    void setPackage(const string&);
    string getBase() const;
    void setBase(const string&);
    string getInterfaces() const;
    void setInterfaces(const string&);
    string getLang() const;
    void setLang(const string&);
    //Method getMethod(const string&, const string&);
    Field getField(const string&);
};

#endif /* CLASS_H_ */
