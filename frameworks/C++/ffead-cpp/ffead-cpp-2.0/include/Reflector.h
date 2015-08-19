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
#ifndef REFLECTOR_H_
#define REFLECTOR_H_
#include "AppDefines.h"
#include "ClassInfo.h"
#include "string"
#include "Method.h"
#include "Field.h"
#include <stdio.h>
#if !defined(OS_MINGW)
#include <sys/wait.h>
#endif
#include <dlfcn.h>
#include <stdexcept>
/*Fix for Windows Cygwin*///#include <execinfo.h>

#include <stdio.h>
#include <stdlib.h>
#include <sstream>
#include <typeinfo>
#include "Constants.h"
#include <iostream>
#include "queue"
#include "deque"
#include "list"
#include "map"
#include "set"
#include "CommonUtils.h"

class Reflector
{
	static ClassInfo nullclass;
	static map<string, ClassInfo> _ciMap;
	bool dlibinstantiated;
	void* dlib;
	vector<string> objectT;
	vector<void*> objects;
	void cleanUp();
public:
	Reflector();
	Reflector(void*);
	virtual ~Reflector();
	const ClassInfo& getClassInfo(const string&, const string& app= "");
	void* getMethodInstance(const Method& method)
	{
		void *mkr = dlsym(dlib, method.getRefName().c_str());
		typedef void* (*RfPtr) (void*,vals);
		RfPtr f = (RfPtr)mkr;
		if(f!=NULL)
		{
			return mkr;
		}
		return NULL;
	}
	template <class T> T invokeMethod(void* instance, const Method& method, const vals& values)
	{
		T obj;
		void *mkr = dlsym(dlib, method.getRefName().c_str());
		typedef void* (*RfPtr) (void*,vals);
		RfPtr f = (RfPtr)mkr;
		if(f!=NULL)
		{
			if(method.getReturnType()!="void")
			{
				void* rt = f(instance,values);
				obj = *(T*)rt;
				delete (T*)rt;
			}
			else
				f(instance,values);
		}
		return obj;
	}
	void destroy(void* instance, const string& cs, const string& app= "")
	{
		ClassInfo ci = getClassInfo(cs, app);
		void *mkr = dlsym(dlib, ci.getDestRefName().c_str());
		typedef void (*RfPtr) (void*);
		RfPtr f = (RfPtr)mkr;
		if(f!=NULL)
		{
			f(instance);
		}
	}
	void* invokeMethodGVP(void* instance, const Method& method, const vals& values)
	{
		void *obj = NULL;
		void *mkr = dlsym(dlib, method.getRefName().c_str());
		typedef void* (*RfPtr) (void*,vals);
		RfPtr f = (RfPtr)mkr;
		if(f!=NULL)
		{
			if(method.getReturnType()!="void")
				obj = f(instance,values);
			else
				f(instance,values);
		}
		return obj;
	}

	template <class T> T newInstance(const Constructor& ctor, const vals& values)
	{
		T obj;
		void *mkr = dlsym(dlib, ctor.getRefName().c_str());
		typedef void* (*RfPtr) (vals);
		RfPtr f = (RfPtr)mkr;
		if(f!=NULL)
		{
			T* objt = (T*)f(values);
			obj = *objt;
			delete objt;
		}
		return *obj;
	}
	template <class T> T newInstance(const Constructor& ctor)
	{
		vals values;
		return newInstance<T>(ctor,values);
	}

	void* newInstanceGVP(const Constructor& ctor, const vals& values)
	{
		void *obj = NULL;
		void *mkr = dlsym(dlib, ctor.getRefName().c_str());
		typedef void* (*RfPtr) (vals);
		RfPtr f = (RfPtr)mkr;
		if(f!=NULL)
		{
			obj = f(values);
		}
		objectT.push_back(ctor.getName());
		objects.push_back(obj);
		return obj;
	}
	void* newInstanceGVP(const Constructor& ctor)
	{
		vals values;
		return newInstanceGVP(ctor,values);
	}

	void* invokeMethodUnknownReturn(void* instance, const Method& method, const vals& values)
	{
		void* obj = NULL;
		void *mkr = dlsym(dlib, method.getRefName().c_str());
		typedef void* (*RfPtr) (void*,vals);
		RfPtr f = (RfPtr)mkr;
		if(f!=NULL)
		{
			if(method.getReturnType()!="void")
				obj = f(instance,values);
			else
				f(instance,values);
		}
		return obj;
	}

	template <class T> T getField(void* instance, const Field& field)
	{
		T t;
		void *mkr = dlsym(dlib, field.getRefName().c_str());
		typedef T (*RfPtr) (void*);
		RfPtr f = (RfPtr)mkr;
		if(f!=NULL)
		{
			t = f(instance);
		}
		return t;
	}
	void* execOperator(void* instance, const string& operato, const vals& values, const string& cs, const string& app= "")
	{
		ClassInfo ci = getClassInfo(cs, app);
		string oprfn = ci.getOperatorRefName(operato);
		void *resul = NULL;
		void *mkr = dlsym(dlib, oprfn.c_str());
		typedef void* (*RfPtr) (void*,vals);
		RfPtr f = (RfPtr)mkr;
		if(f!=NULL)
		{
			resul = f(instance,values);
		}
		return resul;
	}

	void* getNewContainer(const string& cs, const string& contType, const string& app= "")
	{
		ClassInfo ci = getClassInfo(cs, app);
		void *obj = NULL;
		string methodname = ci.getContRefName();
		int t = 1;
		if(contType=="std::set" || contType=="std::multiset") {
			t = 6;
			methodname += "sv";
		}
		void *mkr = dlsym(dlib, methodname.c_str());
		typedef void* (*RfPtr) (void*,void*,int,string,int);
		RfPtr f = (RfPtr)mkr;
		if(f!=NULL)
		{
			obj = f(NULL,NULL,-1,contType,t);
		}
		return obj;
	}
	int getContainerSize(void* vec, const string& cs, const string& contType, const string& app= "")
	{
		ClassInfo ci = getClassInfo(cs, app);
		int size = -1;
		string methodname = ci.getContRefName();
		int t = 2;
		if(contType=="std::set" || contType=="std::multiset") {
			t = 7;
			methodname += "sv";
		}
		void *mkr = dlsym(dlib, methodname.c_str());
		typedef int (*RfPtr) (void*,void*,int,string,int);
		RfPtr f = (RfPtr)mkr;
		if(f!=NULL)
		{
			size = f(vec,NULL,-1,contType,t);
		}
		return size;
	}
	void addToContainer(void* vec, void* instance, const string& cs, const string& contType, const string& app= "")
	{
		ClassInfo ci = getClassInfo(cs, app);
		string methodname = ci.getContRefName();
		int t = 3;
		if(contType=="std::set" || contType=="std::multiset") {
			t = 8;
			methodname += "sv";
		}
		void *mkr = dlsym(dlib, methodname.c_str());
		typedef void (*RfPtr) (void*,void*,int,string,int);
		RfPtr f = (RfPtr)mkr;
		if(f!=NULL)
		{
			f(vec,instance,-1,contType,t);
		}
	}
	void* getContainerElementValueAt(void* vec, const int& pos, const string& cs, const string& contType, const string& app= "")
	{
		ClassInfo ci = getClassInfo(cs, app);
		void *obj = NULL;
		string methodname = ci.getContRefName();
		int t = 4;
		if(contType=="std::set" || contType=="std::multiset") {
			t = 9;
			methodname += "sv";
		}
		void *mkr = dlsym(dlib, methodname.c_str());
		typedef void* (*RfPtr) (void*,void*,int,string,int);
		RfPtr f = (RfPtr)mkr;
		if(f!=NULL)
		{
			obj = f(vec,NULL,pos,contType,t);
		}
		return obj;
	}
	void* getContainerElementAt(void* vec, const int& pos, const string& cs, const string& contType, const string& app= "")
	{
		ClassInfo ci = getClassInfo(cs, app);
		void *obj = NULL;
		string methodname = ci.getContRefName();
		int t = 5;
		if(contType=="std::set" || contType=="std::multiset") {
			t = 10;
			methodname += "sv";
		}
		void *mkr = dlsym(dlib, methodname.c_str());
		typedef void* (*RfPtr) (void*,void*,int,string,int);
		RfPtr f = (RfPtr)mkr;
		if(f!=NULL)
		{
			obj = f(vec,NULL,pos,contType,t);
		}
		return obj;
	}

	template <typename T> static void* getNewNestedContainer(const string& container)
	{
		if(container=="std::vector")
		{
			return new vector<T>;
		}
		else if(container=="std::deque")
		{
			return new deque<T>;
		}
		else if(container=="std::list")
		{
			return new list<T>;
		}
		else if(container=="std::queue")
		{
			return new std::queue<T>;
		}
		return NULL;
	}

	template <typename T> static void* getNewNestedContainerSV(const string& container)
	{
		if(container=="std::set")
		{
			return new set<T>;
		}
		else if(container=="std::multiset")
		{
			return new multiset<T>;
		}
		return NULL;
	}

	template <typename T> static void addValueToNestedContainer(const string& container, const T& t, void* cont)
	{
		if(container.find("std::vector")==0)
		{
			((vector<T>*)cont)->push_back(t);
		}
		else if(container.find("std::deque")==0)
		{
			((deque<T>*)cont)->push_back(t);
		}
		else if(container.find("std::list")==0)
		{
			((list<T>*)cont)->push_back(t);
		}
		else if(container.find("std::queue")==0)
		{
			((std::queue<T>*)cont)->push(t);
		}
	}

	template <typename T> static void addValueToNestedContainerSV(const string& container, const T& t, void* cont)
	{
		if(container.find("std::set")==0)
		{
			((set<T>*)cont)->insert(t);
		}
		else if(container.find("std::multiset")==0)
		{
			((multiset<T>*)cont)->insert(t);
		}
	}

	template <typename T> static int getNestedContainerSize(const string& container, void* cont)
	{
		if(container.find("std::vector")==0)
		{
			return ((vector<T>*)cont)->size();
		}
		else if(container.find("std::deque")==0)
		{
			return ((deque<T>*)cont)->size();
		}
		else if(container.find("std::list")==0)
		{
			return ((list<T>*)cont)->size();
		}
		else if(container.find("std::queue")==0)
		{
			return ((std::queue<T>*)cont)->size();
		}
		return -1;
	}

	template <typename T> static int getNestedContainerSizeSV(const string& container, void* cont)
	{
		if(container.find("std::set")==0)
		{
			return ((set<T>*)cont)->size();
		}
		else if(container.find("std::multiset")==0)
		{
			return ((multiset<T>*)cont)->size();
		}
		return -1;
	}

	template <typename T> static T getValueFromNestedContainer(const string& container, void* cont, const int& pos)
	{
		if(container.find("std::vector")==0)
		{
			return ((vector<T>*)cont)->at(pos);
		}
		else if(container.find("std::deque")==0)
		{
			return ((deque<T>*)cont)->at(pos);
		}
		else if(container.find("std::list")==0)
		{
			typedef typename list<T>::iterator iterator_type;
			iterator_type it;
			it = ((list<T>*)cont)->begin();
			for(int i=0;i<pos;++i, ++it){}
			return *it;
		}
		else if(container.find("std::queue")==0)
		{
			//((std::queue<T>*)cont)->push(t);
		}
		T t;
		return t;
	}

	template <typename T> static void* getPValueFromNestedContainer(const string& container, void* cont, const int& pos)
	{
		if(container.find("std::vector")==0)
		{
			return (void*)&((vector<T>*)cont)->at(pos);
		}
		else if(container.find("std::deque")==0)
		{
			return (void*)&((deque<T>*)cont)->at(pos);
		}
		else if(container.find("std::list")==0)
		{
			typedef typename list<T>::iterator iterator_type;
			iterator_type it;
			it = ((list<T>*)cont)->begin();
			for(int i=0;i<pos;++i, ++it){}
			return (void*)&(*it);
		}
		else if(container.find("std::queue")==0)
		{
			//((std::queue<T>*)cont)->push(t);
		}
		return NULL;
	}

	template <typename T> static T getValueFromNestedContainerSV(const string& container, void* cont, const int& pos)
	{
		if(container.find("std::set")==0)
		{
			typedef typename set<T>::iterator iterator_type;
			iterator_type it;
			it = ((set<T>*)cont)->begin();
			for(int i=0;i<pos;++i, ++it){}
			return *it;
		}
		else if(container.find("std::multiset")==0)
		{
			typedef typename multiset<T>::iterator iterator_type;
			iterator_type it;
			it = ((multiset<T>*)cont)->begin();
			for(int i=0;i<pos;++i, ++it){}
			return *it;
		}
		T t;
		return t;
	}

	template <typename T> static void* getPValueFromNestedContainerSV(const string& container, void* cont, const int& pos)
	{
		if(container.find("std::set")==0)
		{
			typedef typename set<T>::iterator iterator_type;
			iterator_type it;
			it = ((set<T>*)cont)->begin();
			for(int i=0;i<pos;++i, ++it){}
			return (void*)&(*it);
		}
		else if(container.find("std::multiset")==0)
		{
			typedef typename multiset<T>::iterator iterator_type;
			iterator_type it;
			it = ((multiset<T>*)cont)->begin();
			for(int i=0;i<pos;++i, ++it){}
			return (void*)&(*it);
		}
		return NULL;
	}
};
#endif
