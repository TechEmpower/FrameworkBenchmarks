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
 * BinarySerialize.h
 *
 *  Created on: 12-Jun-2013
 *      Author: sumeetc
 */

#ifndef BINARYSERIALIZE_H_
#define BINARYSERIALIZE_H_
#include "SerializeBase.h"
#include "AMEFResources.h"

typedef string (*SerPtr) (void*);
typedef void* (*UnSerPtr) (const string&);

class BinarySerialize : public SerializeBase {

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
	BinarySerialize();
	BinarySerialize(void*);
	~BinarySerialize();

	template <class T> static string serialize(T& t, const string& appName = "")
	{
		BinarySerialize serialize;
		string className = getClassName(t);
		return _handleAllSerialization(className,&t,appName, &serialize);
	}
	template <class T> static string serializePointer(T* t, const string& appName = "")
	{
		BinarySerialize serialize;
		string className = getClassName(t);
		return _handleAllSerialization(className,t,appName, &serialize);
	}
	static string serializeUnknown(void* t, const string& className, const string& appName = "");

	template <class K,class V> static string serializeMap(map<K,V>& mp, const string& appName = "")
	{
		typedef typename map<K,V>::iterator kvmapiter;
		AMEFEncoder enc;
		AMEFObject object;
		K k;
		string kclassName = getClassName(k);
		V v;
		string vclassName = getClassName(v);
		kclassName = "map<"+kclassName+":"+vclassName+">";
		object.setName(kclassName);
		kvmapiter it;
		for (it=mp.begin();it!=mp.end();++it)
		{
			//string key = serialize<K>(it->first,appName);
			string value = serialize<V>(it->second,appName);
			object.addPacket(value, CastUtil::lexical_cast<string>(it->first));
		}
		return enc.encodeB(&object);
	}
	template <class K,class V> static map<K,V> unSerializeToMap(const string& serStr, const string& appName = "")
	{
		map<K,V> mp;
		AMEFDecoder dec;
		AMEFObject object;
		AMEFObject* root = dec.decodeB(serStr, true);
		if(root!=NULL)
		{
			for (int var = 0; var < (int)root->getPackets().size(); var++)
			{
				V val = unserialize<V>(root->getPackets().at(var)->getValueStr(), appName);
				K key = CastUtil::lexical_cast<K>(root->getPackets().at(var)->getNameStr());
				mp[key] = val;
			}
		}
		return mp;
	}

	template <class K,class V> static string serializeMultiMap(multimap<K,V>& mp, const string& appName = "")
	{
		typedef typename multimap<K,V>::iterator kvmapiter;
		AMEFEncoder enc;
		AMEFObject object;
		K k;
		string kclassName = getClassName(k);
		V v;
		string vclassName = getClassName(v);
		kclassName = "multimap<"+kclassName+":"+vclassName+">";
		object.setName(kclassName);
		kvmapiter it;
		for (it=mp.begin();it!=mp.end();++it)
		{
			//string key = serialize<K>(it->first,appName);
			string value = serialize<V>(it->second,appName);
			object.addPacket(value, CastUtil::lexical_cast<string>(it->first));
		}
		return enc.encodeB(&object);
	}
	template <class K,class V> static multimap<K,V> unSerializeToMultiMap(const string& serStr, const string& appName = "")
	{
		multimap<K,V> mp;
		AMEFDecoder dec;
		AMEFObject object;
		AMEFObject* root = dec.decodeB(serStr, true);
		if(root!=NULL)
		{
			for (int var = 0; var < (int)root->getPackets().size(); var++)
			{
				V val = unserialize<V>(root->getPackets().at(var)->getValueStr(), appName);
				K key = CastUtil::lexical_cast<K>(root->getPackets().at(var)->getNameStr());
				mp[key] = val;
			}
		}
		return mp;
	}


	template <class T> static T unserialize(const string& objXml, const string& appName = "")
	{
		BinarySerialize serialize;
		T t;
		string className = getClassName(t);
		T* tp = (T*)_handleAllUnSerialization(objXml,className,appName,&serialize,false,NULL);
		if(tp!=NULL)
		{
			t = *(T*)tp;
			delete ((T*)tp);
		}
		return t;
	}
	template <class T> static T unserialize(AMEFObject* serObject, const string& appName = "")
	{
		BinarySerialize serialize;
		T t;
		string className = getClassName(t);
		T* tp = (T*)_handleAllUnSerialization("",className,appName,&serialize,false,serObject);
		if(tp!=NULL)
		{
			t = *(T*)tp;
			delete ((T*)tp);
		}
		return t;
	}
	template <class T> static T* unserializeToPointer(const string& objXml, const string& appName = "")
	{
		BinarySerialize serialize;
		T* t;
		string className = getClassName(t);
		return (T*)_handleAllUnSerialization(objXml,className,appName,&serialize,false,NULL);
	}
	template <class T> static T* unserializeToPointer(AMEFObject* serObject, const string& appName = "")
	{
		BinarySerialize serialize;
		T* t;
		string className = getClassName(t);
		return (T*)_handleAllUnSerialization("",className,appName,&serialize,false,serObject);
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

#endif /* BINARYSERIALIZE_H_ */
