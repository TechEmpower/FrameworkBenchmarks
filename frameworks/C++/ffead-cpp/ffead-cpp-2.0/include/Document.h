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
#ifndef DOCUMENT_H_
#define DOCUMENT_H_
#include "Element.h"

class Document : public Renderer {
public:
	Document();
	virtual ~Document();
	Element& getRootElement();
	void setRootElement(const Element&);
	Element* getElementByName(const string&);
	string render();
    const string& getDocType() const;
    void setDocType(const string&);
private:
    static Element nullele;
	Element root;
	string docType;
	Element* getElementByName(const string&, Element*);
	friend class XmlParser;
};

#endif

