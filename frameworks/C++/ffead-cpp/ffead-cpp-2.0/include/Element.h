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

#ifndef ELEMENT_H_
#define ELEMENT_H_


#include "map"
#include "vector"
#include "StringUtil.h"
#include "string"
#include "Renderer.h"
#include <iostream>
using namespace std;

class Element;
typedef vector<Element*> ElementList;
typedef map<string, string> AttributeList;
typedef map<string, Element*> ElementMap;

class Element : public Renderer
{
public:
	Element();
	~Element();

	//typedef vector<Element*> ElementList;
	void addElement(Element*);
	void removeElement(Element*);
	//void updateElement(Element*);
	void addAttribute(const string& key, const string& value, const bool& validate= false);
	void removeAttribute(const string& key);
	const AttributeList& getAttributes() const;
	const string getAttribute(const string&) const;
	const ElementList& getChildElements() const;
	bool isNull();
	const string& getTagName() const;
	string getTagNameSpc() const;
	const string& getNameSpc();
	void setTagName(const string& tagName);
	bool operator == (const Element&) const;
	bool operator == (Element *);
	bool getCdata() const;
	void setCdata(const bool&);
	const string& getText() const;
	void setText(const string&);
	Element* getElementByName(const string&);
	Element* getElementByNameIgnoreCase(const string&);
	ElementList getElementsByName(const string& name);
	string render();
	string renderSerialization();
	string renderChildren() const;
	void validateNs();
	string getNameSpcValue();
	void copy(Element* to);
private:
	static Element nullele;
	bool isValidNamespace(Element* ele, const string& nameSpace);
	string getNmspc(Element* ele, const string& nameSpace);
	Element* parent;
	AttributeList attributes;
	AttributeList namespaces;
	AttributeList allnmspcs;
	ElementList elements;
	string tagName;
	string nameSpace;
	string text;
	bool cdata;
	ElementMap mapOfEle;
	friend class Document;
};
#endif


