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
 * JSONSerialize.h
 *
 *  Created on: 12-Jun-2013
 *      Author: sumeetc
 */

#ifndef JSONSERIALIZE_H_
#define JSONSERIALIZE_H_
#include "SerializeBase.h"
#include "JSONUtil.h"

class JSONSerialize : public SerializeBase {

	string serializePrimitive(const string& className, void* t);
	void* getSerializableObject();
	void cleanSerializableObject(void* _1);
	void startContainerSerialization(void* _1, const string& className, const string& container);
	void endContainerSerialization(void* _1, const string& className, const string& container);
	void afterAddContainerSerializableElement(void* _1, const int& counter, const int& size);
	void addContainerSerializableElement(void* _1, const string& tem);
	void addContainerSerializableElementMulti(void* _1, const string& tem);
	string fromSerializableObjectToString(void* _1);
	string elementToSerializedString(void* _1, const int& counter);
	string getConatinerElementClassName(void* _1, const string& className);
	void* getContainerElement(void* _1, const int& counter, const int& counter1= -1);
	void addPrimitiveElementToContainer(void* _1, const int& counter, const string& className, void* cont, const string& container);
	void* getUnserializableObject(const string& _1);
	void cleanUnserializableObject(void* _1);
	void cleanValidUnserializableObject(void* _1);
	void* getValidUnserializableObject(const string& _1);
	int getContainerSize(void* _1);
	string getUnserializableClassName(void* _1, const string& className);
	void* getPrimitiveValue(void* _1, const string& className);
public:
	JSONSerialize();
	JSONSerialize(void*);
	~JSONSerialize();

	template <class T> static string serialize(T& t, const string& appName = "")
	{
		JSONSerialize serialize;
		string className = CastUtil::getClassName(t);
		return _handleAllSerialization(className,&t,appName,&serialize);
	}
	template <class T> static string serializePointer(T* t, const string& appName = "")
	{
		JSONSerialize serialize;
		string className = CastUtil::getClassName(t);
		return _handleAllSerialization(className,t,appName,&serialize);
	}
	static string serializeUnknown(void* t, const string& className, const string& appName = "");

	/*template <class K,class V> static string serialize(const map<K,V>& mp, const string& appName = "")
	{
		map<K,V> mpt  = mp;
		AMEFEncoder enc;
		AMEFObject object;
		K k;
		string kclassName = CastUtil::getClassName(k);
		V v;
		string serval;
		string vclassName = CastUtil::getClassName(v);
		kclassName = "map<"+kclassName+":"+vclassName+">";
		object.setName(kclassName);
		while (mpt.begin()!=mpt.end())
		{
			string key = serialize<K>(mpt.begin()->first,appName);
			string value = serialize<V>(mpt.begin()->second,appName);
			mpt.erase(mpt.begin());
			object.addPacket(value, key);
		}
		return enc.encodeB(&object, false);
	}

	template <class K,class V> static string serialize(const multimap<K,V>& mp, const string& appName = "")
	{
		multimap<K,V> mpt  = mp;
		AMEFEncoder enc;
		AMEFObject object;
		K k;
		string kclassName = CastUtil::getClassName(k);
		V v;
		string serval;
		string vclassName = CastUtil::getClassName(v);
		kclassName = "multimap<"+kclassName+":"+vclassName+">";
		object.setName(kclassName);
		while (mpt.begin()!=mpt.end())
		{
			string key = serialize<K>(mpt.begin()->first,appName);
			string value = serialize<V>(mpt.begin()->second,appName);
			mpt.erase(mpt.begin());
			object.addPacket(value, key);
		}
		return enc.encodeB(&object, false);
	}*/
	template <class T> static T unserialize(const string& objXml, const string& appName = "")
	{
		JSONSerialize serialize;
		T t;
		string className = CastUtil::getClassName(t);
		T* tp = (T*)_handleAllUnSerialization(objXml,className,appName,&serialize,true,NULL);
		if(tp!=NULL)
		{
			t = *(T*)tp;
			delete ((T*)tp);
		}
		return t;
	}
	template <class T> static T unserialize(JSONElement* element, const string& appName = "")
	{
		JSONSerialize serialize;
		T t;
		string className = CastUtil::getClassName(t);
		T* tp = (T*)_handleAllUnSerialization("",className,appName,&serialize,true,element);
		if(tp!=NULL)
		{
			t = *(T*)tp;
			delete ((T*)tp);
		}
		return t;
	}
	template <class T> static T* unserializeToPointer(const string& objXml, const string& appName = "")
	{
		JSONSerialize serialize;
		T* t;
		string className = CastUtil::getClassName(t);
		return (T*)_handleAllUnSerialization(objXml,className,appName,&serialize,true,NULL);
	}
	template <class T> static T* unserializeToPointer(JSONElement* element, const string& appName = "")
	{
		JSONSerialize serialize;
		T* t;
		string className = CastUtil::getClassName(t);
		return (T*)_handleAllUnSerialization("",className,appName,&serialize,true,element);
	}

	bool isValidClassNamespace(void* _1, const string& className, const string& namespc, const bool& iscontainer= false);
	bool isValidObjectProperty(void* _1, const string& propname, const int& counter);
	void* getObjectProperty(void* _1, const int& counter);
	void startObjectSerialization(void* _1, const string& className);
	void endObjectSerialization(void* _1, const string& className);
	void afterAddObjectProperty(void* _1);
	void addObjectPrimitiveProperty(void* _1, const string& propName, const string& className, void* t);
	void addObjectProperty(void* _1, const string& propName, string className, const string& t);
	void* getObjectPrimitiveValue(void* _1, const string& className, const string& propName);
	static void* unSerializeUnknown(const string& objXml, const string& className, const string& appName = "");
	string serializeUnknownBase(void* t, const string& className, const string& appName = "");
	void* unSerializeUnknownBase(void* unserObj, const string& className, const string& appName = "");
	void* unSerializeUnknownBase(const string& serVal, const string& className, const string& appName = "");
};

#endif /* JSONSERIALIZE_H_ */
