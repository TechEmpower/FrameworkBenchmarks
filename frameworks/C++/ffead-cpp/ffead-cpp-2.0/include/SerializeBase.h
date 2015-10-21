/*
	Copyright 2010, Sumeet Chhetri

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
 * SerializeBase.h
 *
 *  Created on: 09-Jun-2013
 *      Author: sumeetc
 */

#ifndef SERIALIZEBASE_H_
#define SERIALIZEBASE_H_
#include "AppDefines.h"
#include "CastUtil.h"
#include <stdexcept>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include "string"
#include <sstream>
#include <typeinfo>
#include "queue"
#include "deque"
#include "list"
#include "map"
#include "set"
#include "stack"
#include "DateFormat.h"
#include "BinaryData.h"
#include "Constants.h"
#include "CommonUtils.h"

class Dummy{
public:
	bool operator <(const Dummy& du) const
	{
		return true;
	}
};

class DummyList
{
public:
	DummyList(){}
	struct _List_node_base
	{
		_List_node_base* _M_next;
		_List_node_base* _M_prev;
	};
	struct _List_impl
	{
		_List_node_base _M_node;
	};
	_List_impl _M_impl;
	friend class SerializeBase;
public:
	bool operator <(const DummyList& du) const
	{
		return true;
	}
};

class DummyDeque
{
	struct Dummy_Deque_iterator
	{
	  void* _M_cur;
	  void* _M_first;
	  void* _M_last;
	  void** _M_node;
	};
	struct _Deque_impl
	{
		void** _M_map;
		size_t _M_map_size;
		Dummy_Deque_iterator _M_start;
		Dummy_Deque_iterator _M_finish;
	};
	_Deque_impl _M_impl;
	friend class SerializeBase;
public:
	bool operator <(const DummyDeque& du) const
	{
		return true;
	}
};

class DummyQueue
{
	DummyDeque dq;
	friend class SerializeBase;
	friend class Serialize;
public:
	bool operator <(const DummyQueue& du) const
	{
		return true;
	}
};

class DummySet
{
	struct _Rb_tree_node_base
	{
	    typedef _Rb_tree_node_base* _Base_ptr;
	    _Rb_tree_color	_M_color;
	    _Base_ptr		_M_parent;
	    _Base_ptr		_M_left;
	    _Base_ptr		_M_right;
	};
	struct less
	{
	  bool
	  operator()(const int& __x, const int& __y) const
	  { return __x < __y; }
	};
	struct _Rb_tree_impl
	{
	  less		_M_key_compare;
	  _Rb_tree_node_base 	_M_header;
	  size_t 		_M_node_count; // Keeps track of size of tree.
	};
	struct _Rep_type
	{
		_Rb_tree_impl _M_impl;
	};
	_Rep_type _M_t;
	friend class SerializeBase;
public:
	bool operator <(const DummySet& du) const
	{
		return true;
	}
};

class DummyVec
{
	struct _Vector_impl
	{
		void* _M_start;
		void* _M_finish;
		void* _M_end_of_storage;
	};
	_Vector_impl _M_impl;
	friend class SerializeBase;
public:
	bool operator <(const DummyVec& du) const
	{
		return true;
	}
};

class SerializeBase {
	static map<string, string> _mangledClassNameMap;
protected:
	bool dlibinstantiated;
	void* dlib;
	static void processClassName(string& className);
	static void processClassAndContainerNames(string& className, string& container);
public:
	template <typename T> static string getClassName(const T& t)
	{
		const char *mangled = typeid(t).name();
		string sm(mangled);
		if(_mangledClassNameMap.find(sm)!=_mangledClassNameMap.end()) {
			string tn = _mangledClassNameMap[sm];
			return tn;
		}
		int status;
		char *demangled;
		using namespace abi;
		demangled = __cxa_demangle(mangled, NULL, 0, &status);
		string tn(demangled);
		free(demangled);
		_mangledClassNameMap[sm] = tn;
		return tn;
	}
	template <class T> static string serializeset(const set<T>& t, const string& app, SerializeBase* base)
	{
		T tr;
		string appName = CommonUtils::getAppName(app);
		int cnt = 0;
		int size = t.size();
		typedef typename set<T>::iterator iterator_type;
		iterator_type it;
		string className = getClassName(tr);
		void* object = base->getSerializableObject();
		base->startContainerSerialization(object, className, "std::set");
		for(it = t.begin(); it!= t.end(); ++it)
		{
			base->addContainerSerializableElement(object, serialize<T>(*it, appName, base));
			base->afterAddContainerSerializableElement(object, cnt++, size);
		}
		base->endContainerSerialization(object, className, "std::set");
		string ser = base->fromSerializableObjectToString(object);
		base->cleanSerializableObject(object);
		return ser;
	}
	template <class T> static string serializemultiset(const multiset<T>& t, const string& app, SerializeBase* base)
	{
		T tr;
		string appName = CommonUtils::getAppName(app);
		int cnt = 0;
		int size = t.size();
		typedef typename multiset<T>::iterator iterator_type;
		iterator_type it;
		string className = getClassName(tr);
		void* object = base->getSerializableObject();
		base->startContainerSerialization(object, className, "std::multiset");
		for(it = t.begin(); it!= t.end(); ++it)
		{
			base->addContainerSerializableElement(object, serialize<T>(*it, appName, base));
			base->afterAddContainerSerializableElement(object, cnt++, size);
		}
		base->endContainerSerialization(object, className, "std::multiset");
		string ser = base->fromSerializableObjectToString(object);
		base->cleanSerializableObject(object);
		return ser;
	}
	template <class T> static string serializevec(const vector<T>& t, const string& app, SerializeBase* base)
	{
		T tr;
		string appName = CommonUtils::getAppName(app);
		int size = t.size();
		string className = getClassName(tr);
		void* object = base->getSerializableObject();
		base->startContainerSerialization(object, className, "std::vector");
		for(int var = 0; var < (int)t.size(); ++var)
		{
			base->addContainerSerializableElement(object, serialize<T>(t.at(var), appName, base));
			base->afterAddContainerSerializableElement(object, var, size);
		}
		base->endContainerSerialization(object, className, "std::vector");
		string ser = base->fromSerializableObjectToString(object);
		base->cleanSerializableObject(object);
		return ser;
	}
	template <class T> static string serializedq(const deque<T>& t, const string& app, SerializeBase* base)
	{
		T tr;
		string appName = CommonUtils::getAppName(app);
		int size = t.size();
		string className = getClassName(tr);
		void* object = base->getSerializableObject();
		base->startContainerSerialization(object, className, "std::deque");
		for(int var = 0; var < (int)t.size(); ++var)
		{
			base->addContainerSerializableElement(object, serialize<T>(t.at(var), appName, base));
			base->afterAddContainerSerializableElement(object, var, size);
		}
		base->endContainerSerialization(object, className, "std::deque");
		string ser = base->fromSerializableObjectToString(object);
		base->cleanSerializableObject(object);
		return ser;
	}
	template <class T> static string serializelist(const list<T>& t, const string& app, SerializeBase* base)
	{
		T tr;
		string appName = CommonUtils::getAppName(app);
		int cnt = 0;
		int size = t.size();
		typedef typename list<T>::const_iterator iterator_type;
		iterator_type it;
		string className = getClassName(tr);
		void* object = base->getSerializableObject();
		base->startContainerSerialization(object, className, "std::list");
		for(it = t.begin(); it!= t.end(); ++it)
		{
			base->addContainerSerializableElement(object, serialize<T>(*it, appName, base));
			base->afterAddContainerSerializableElement(object, cnt, size);
		}
		base->endContainerSerialization(object, className, "std::list");
		string ser = base->fromSerializableObjectToString(object);
		base->cleanSerializableObject(object);
		return ser;
	}
	template <class T> static string serializeq(const std::queue<T>& t, const string& app, SerializeBase* base)
	{
		T tr;
		string appName = CommonUtils::getAppName(app);
		DummyQueue* dptr = (DummyQueue*)&t;
		deque<T>* tt = (deque<T>*)&dptr->dq;
		string className = getClassName(tr);
		void* object = base->getSerializableObject();
		base->startContainerSerialization(object, className, "std::queue");
		for(int var = 0; var < (int)tt->size(); ++var)
		{
			base->addContainerSerializableElement(object, serialize<T>(tt->at(var), appName, base));
			base->afterAddContainerSerializableElement(object, var, (int)tt->size());
		}
		base->endContainerSerialization(object, className, "std::queue");
		string ser = base->fromSerializableObjectToString(object);
		base->cleanSerializableObject(object);
		return ser;
	}
	template <class T> static string serialize(T t, const string& app, SerializeBase* base)
	{
		string appName = CommonUtils::getAppName(app);
		string className = getClassName(t);
		return base->serializeUnknownBase(&t, className, appName);
	}
	template <class T> static string serializePointer(T* t, const string& app, SerializeBase* base)
	{
		string appName = CommonUtils::getAppName(app);
		string className = getClassName(t);
		return base->serializeUnknownBase(t, className, appName);
	}
	static string serializeUnknown(void* t, const string& className, const string& appName, SerializeBase* base);
	template <class T> static T unSerialize(void* unserObj, const string& app, SerializeBase* base)
	{
		string appName = CommonUtils::getAppName(app);
		T t;
		string className = getClassName(t);
		void* retVP = base->unSerializeUnknownBase(unserObj, className, appName);
		if(retVP!=NULL)
		{
			t = *(T*)retVP;
			delete ((T*)retVP);
		}
		return t;
	}
	template <class T> static T* unSerializeToPointer(void* unserObj, const string& app, SerializeBase* base)
	{
		string appName = CommonUtils::getAppName(app);
		T t;
		string className = getClassName(t);
		void* retVP = base->unSerializeUnknownBase(unserObj, className, appName);
		return (T*)retVP;
	}
	static void* unSerializeUnknown(const string& objXml, const string& className, const string& appName, SerializeBase* base);
	static void* unSerializeUnknown(void* unserObj, const string& className, const string& appName, SerializeBase* base);
	template <class T> static T* unSerializeKnownToPointer(void* unserObj, const string& className, const string& app, SerializeBase* base)
	{
		string appName = CommonUtils::getAppName(app);
		void* retVP = base->unSerializeUnknownBase(unserObj, className, appName);
		return (T*)retVP;
	}
	template <class T> static T unSerializeKnown(void* unserObj, const string& className, const string& app, SerializeBase* base)
	{
		T t;
		string appName = CommonUtils::getAppName(app);
		void* retVP = base->unSerializeUnknownBase(unserObj, className, appName);
		if(retVP!=NULL)
		{
			t = *(T*)retVP;
			delete ((T*)retVP);
		}
		return t;
	}
	template <class T> static T* unSerializeKnownToPointer(const string& objXml, const string& className, const string& app, SerializeBase* base)
	{
		string appName = CommonUtils::getAppName(app);
		void* retVP = base->unSerializeUnknownBase(objXml, className, appName);
		return (T*)retVP;
	}
	template <class T> static T unSerializeKnown(const string& objXml, const string& className, const string& app, SerializeBase* base)
	{
		T t;
		string appName = CommonUtils::getAppName(app);
		void* retVP = base->unSerializeUnknownBase(objXml, className, appName);
		if(retVP!=NULL)
		{
			t = *(T*)retVP;
			delete ((T*)retVP);
		}
		return t;
	}


	virtual string serializeUnknownBase(void* t, const string& className, const string& app) = 0;
	virtual void* unSerializeUnknownBase(void* unserObj, const string& className, const string& app) = 0;
	virtual void* unSerializeUnknownBase(const string& serVal, const string& className, const string& app) = 0;
	static string _serContainer(void* t, const string& className, const string& app, const string& type, SerializeBase* base);
	static string _ser(void* t, const string& className, const string& app, SerializeBase* base);
	static void* _unserContainer(void* unserableObject, const string& className, const string& app, const string& type, SerializeBase* base);
	static void* _unser(void* unserableObject, const string& className, const string& app, SerializeBase* base);
	string getSerializationMethodName(const string& className, const string& app, const bool& which, const string& type= "");

	//Base Methods for serialization, implementation in Serilaize, XMLSerialize and JSONSerialize
	virtual void* getSerializableObject() = 0;
	virtual void cleanSerializableObject(void* _1) = 0;
	virtual void startContainerSerialization(void* _1, const string& className, const string& container) = 0;
	virtual void endContainerSerialization(void* _1, const string& className, const string& container) = 0;
	virtual void afterAddContainerSerializableElement(void* _1, const int& counter, const int& size) = 0;
	virtual void addContainerSerializableElement(void* _1, const string& tem) = 0;
	virtual void addContainerSerializableElementMulti(void* _1, const string& tem) = 0;
	virtual string fromSerializableObjectToString(void* _1) = 0;
	virtual string serializePrimitive(const string& className, void* t) = 0;
	virtual string elementToSerializedString(void* _1, const int& counter) = 0;
	virtual void startObjectSerialization(void* _1, const string& className) = 0;
	virtual void endObjectSerialization(void* _1, const string& className) = 0;
	virtual void afterAddObjectProperty(void* _1) = 0;
	virtual void addObjectPrimitiveProperty(void* _1, const string& propName, const string& className, void* t) = 0;
	virtual void addObjectProperty(void* _1, const string& propName, string className, const string& t) = 0;

	virtual bool isValidClassNamespace(void* _1, const string& classname, const string& namespc, const bool& iscontainer= false) = 0;
	virtual bool isValidObjectProperty(void* _1, const string& propname, const int& counter) = 0;
	virtual void* getObjectProperty(void* _1, const int& counter) = 0;
	virtual void* getObjectPrimitiveValue(void* _1, const string& className, const string& propName) = 0;

	//
	virtual string getUnserializableClassName(void* _1, const string& className) = 0;
	virtual void* getPrimitiveValue(void* _1, const string& className) = 0;
	virtual void* getContainerElement(void* _1, const int& counter, const int& counter1= -1) = 0;
	virtual void* getUnserializableObject(const string& _1) = 0;
	virtual void cleanUnserializableObject(void* _1) = 0;
	virtual void cleanValidUnserializableObject(void* _1) = 0;
	virtual void* getValidUnserializableObject(const string& _1) = 0;
	virtual int getContainerSize(void* _1) = 0;
	virtual string getConatinerElementClassName(void* _1, const string& className) = 0;
	virtual void addPrimitiveElementToContainer(void* _1, const int& counter, const string& className, void* cont, const string& container) = 0;

	static string _handleAllSerialization(string className, void *t, const string& app, SerializeBase* base);
	static void* _handleAllUnSerialization(const string& serVal, string className, const string& app, SerializeBase* base, const bool& isJsonSer, void* serObject);

	static string handleMultiLevelSerialization(void* t, string className, const string& app, SerializeBase* base);
	static void* handleMultiLevelUnSerialization(void* root, string className, const string& app, int& size, SerializeBase* base);

	static string getTypeName(const string& type);
	static bool isPrimitiveDataType(const string& type);
	static string getTemplateArg(const string& s, string& tem);
	static void addToNestedContainer(void* roott, const string& className, const string& app, int& lsiz, const string& container, void* cont, const int& var, SerializeBase* base);
	static void* getNestedContainer(string& className);

	static void* unserializevec(void* unserableObject, const string& app, int &size, SerializeBase* base, const string& classN);
	static void* unserializelist(void* unserableObject, const string& app, int &size, SerializeBase* base, const string& classN);
	static void* unserializeset(void* unserableObject, const string& app, int &size, SerializeBase* base, const string& classN);
	static void* unserializemultiset(void* unserableObject, const string& app, int &size, SerializeBase* base, const string& classN);
	static void* unserializeq(void* unserableObject, const string& app, int &size, SerializeBase* base, const string& classN);
	static void* unserializedq(void* unserableObject, const string& app, int &size, SerializeBase* base, const string& classN);

	virtual ~SerializeBase();

	template <typename T> static void* getNewNestedContainer(const string& container)
	{
		if(container=="std::vector" || container=="vector")
		{
			return new vector<T>;
		}
		else if(container=="std::deque" || container=="deque")
		{
			return new deque<T>;
		}
		else if(container=="std::list" || container=="list")
		{
			return new list<T>;
		}
		else if(container=="std::queue" || container=="queue")
		{
			return new std::queue<T>;
		}
		return NULL;
	}

	template <typename T> static void addValueToNestedContainer(const string& container, const T& t, void* cont)
	{
		if(container.find("std::vector")==0 || container.find("vector")==0)
		{
			((vector<T>*)cont)->push_back(t);
		}
		else if(container.find("std::deque")==0 || container.find("deque")==0)
		{
			((deque<T>*)cont)->push_back(t);
		}
		else if(container.find("std::list")==0 || container.find("list")==0)
		{
			((list<T>*)cont)->push_back(t);
		}
		else if(container.find("std::queue")==0 || container.find("queue")==0)
		{
			((std::queue<T>*)cont)->push(t);
		}
	}

	template <typename T> static int getNestedContainerSize(const string& container, void* cont)
	{
		if(container.find("std::vector")==0 || container.find("vector")==0)
		{
			return ((vector<T>*)cont)->size();
		}
		else if(container.find("std::deque")==0 || container.find("deque")==0)
		{
			return ((deque<T>*)cont)->size();
		}
		else if(container.find("std::list")==0 || container.find("list")==0)
		{
			return ((list<T>*)cont)->size();
		}
		else if(container.find("std::queue")==0 || container.find("queue")==0)
		{
			return ((std::queue<T>*)cont)->size();
		}
		return -1;
	}

	template <typename T> static T getValueFromNestedContainer(const string& container, void* cont, const int& pos)
	{
		if(container.find("std::vector")==0 || container.find("vector")==0)
		{
			return ((vector<T>*)cont)->at(pos);
		}
		else if(container.find("std::deque")==0 || container.find("deque")==0)
		{
			return ((deque<T>*)cont)->at(pos);
		}
		else if(container.find("std::list")==0 || container.find("list")==0)
		{
			typedef typename list<T>::iterator iterator_type;
			iterator_type it;
			it = ((list<T>*)cont)->begin();
			for(int i=0;i<pos;++i, ++it){}
			return *it;
		}
		else if(container.find("std::queue")==0 || container.find("queue")==0)
		{
			//((std::queue<T>*)cont)->push(t);
		}
		T t;
		return t;
	}

	template <typename T> static void* getNewNestedContainerSV(const string& container)
	{
		if(container=="std::set" || container=="set")
		{
			return new set<T>;
		}
		else if(container=="std::multiset" || container=="multiset")
		{
			return new multiset<T>;
		}
		return NULL;
	}

	template <typename T> static void addValueToNestedContainerSV(const string& container, const T& t, void* cont)
	{
		if(container.find("std::set")==0 || container.find("set")==0)
		{
			((set<T>*)cont)->insert(t);
		}
		else if(container.find("std::multiset")==0 || container.find("multiset")==0)
		{
			((multiset<T>*)cont)->insert(t);
		}
	}

	template <typename T> static int getNestedContainerSizeSV(const string& container, void* cont)
	{
		if(container.find("std::set")==0 || container.find("set")==0)
		{
			return ((set<T>*)cont)->size();
		}
		else if(container.find("std::multiset")==0 || container.find("multiset")==0)
		{
			return ((multiset<T>*)cont)->size();
		}
		return -1;
	}

	template <typename T> static T getValueFromNestedContainerSV(const string& container, void* cont, const int& pos)
	{
		if(container.find("std::set")==0 || container.find("set")==0)
		{
			typedef typename set<T>::iterator iterator_type;
			iterator_type it;
			it = ((set<T>*)cont)->begin();
			for(int i=0;i<pos;++i, ++it){}
			return *it;
		}
		else if(container.find("std::multiset")==0 || container.find("multiset")==0)
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
};

#endif /* SERIALIZEBASE_H_ */
