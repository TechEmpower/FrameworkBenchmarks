/*
 * XMLSerialize.h
 *
 *  Created on: 12-Jun-2013
 *      Author: sumeetc
 */

#ifndef XMLSERIALIZE_H_
#define XMLSERIALIZE_H_
#include "SerializeBase.h"
#include "XmlParser.h"

class XMLSerialize : public SerializeBase {

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
	XMLSerialize();
	XMLSerialize(void*);
	~XMLSerialize();

	template <class T> static string serialize(T& t, const string& appName = "")
	{
		XMLSerialize serialize;
		string className = getClassName(t);
		return _handleAllSerialization(className,&t,appName, &serialize);
	}
	template <class T> static string serialize(void* t, const string& className, const string& appName = "")
	{
		XMLSerialize serialize;
		return _handleAllSerialization(className,t,appName, &serialize);
	}
	template <class T> static string serializePointer(T* t, const string& appName = "")
	{
		XMLSerialize serialize;
		string className = getClassName(t);
		return _handleAllSerialization(className,t,appName, &serialize);
	}
	static string serializeUnknown(void* t, const string& className, const string& appName = "");

	template <class K,class V> static string serializeMap(const map<K,V>& mp, const string& appName = "")
	{
		map<K,V> mpt  = mp;
		K k;
		string kclassName = getClassName(k);
		V v;
		string serval;
		string vclassName = getClassName(v);
		kclassName = "map-"+kclassName+"-"+vclassName;
		serval = "<" + kclassName + ">";
		while (mpt.begin()!=mpt.end())
		{
			string key = serialize<K>(mpt.begin()->first,appName);
			string value = serialize<V>(mpt.begin()->second,appName);
			serval += "<entry><key>" + key + "</key>";
			serval += "<value>" + value + "</value></entry>";
			mpt.erase(mpt.begin());
		}
		serval = "</" + kclassName + ">";
		return serval;
	}
	template <class K,class V> static map<K,V> unSerializeToMap(const string& serStr, const string& appName = "")
	{
		map<K,V> mp;
		XmlParser parser("Parser");
		try
		{
			Document doc;
			parser.parse(serStr, doc);
			Element message = doc.getRootElement();
			if(message.getTagName()!="" && message.getTagName().find("map-")!=string::npos
					&& message.getChildElements().size()>0)
			{
				string maptype = message.getTagName();
				string keytype = maptype.substr(maptype.find("-")+1);
				if(keytype.find("-")==string::npos)
					return mp;
				string valtype = keytype.substr(keytype.find("-")+1);
				keytype = keytype.substr(0, keytype.find("-"));
				for (int var = 0; var < (int)message.getChildElements().size(); var++)
				{
					Element entry = message.getChildElements().at(var);
					if(entry.getTagName()=="entry")
					{
						Element key = entry.getElementByName("key");
						Element value = entry.getElementByName("value");
						if(key.getTagName()!="" && value.getTagName()!="" && key.getChildElements().size()>0
								&& value.getChildElements().size()>0) {
							K k = unserialize<K>(&key.getChildElements().at(0), keytype);
							V v = unserialize<K>(&value.getChildElements().at(0), valtype);
							mp[k] = v;
						} else if(key.getTagName()!="" && key.getChildElements().size()>0) {
							K k = unserialize<K>(&key.getChildElements().at(0), keytype);
							V v;
							mp[k] = v;
						}
					}
				}
			}
		} catch(const XmlParseException& str) {
			cout << str.getMessage() << endl;
		} catch(...) {
			cout << "XML Parse Error" << endl;
		}
		return mp;
	}

	template <class K,class V> static string serializeMultimap(multimap<K,V>& mp, const string& appName = "")
	{
		multimap<K,V> mpt  = mp;
		K k;
		string kclassName = getClassName(k);
		V v;
		string serval;
		string vclassName = getClassName(k);
		kclassName = "multimap-"+kclassName+"-"+vclassName;
		serval = "<" + kclassName + ">";
		while (mpt.begin()!=mpt.end())
		{
			string key = serialize<K>(mpt.begin()->first,appName);
			string value = serialize<V>(mpt.begin()->second,appName);
			serval += "<entry><key>" + key + "</key>";
			serval += "<value>" + value + "</value></entry>";
			mpt.erase(mpt.begin());
		}
		serval = "</" + kclassName + ">";
		return serval;
	}
	template <class K,class V> static map<K,V> unSerializeToMultiMap(const string& serStr, const string& appName = "")
	{
		multimap<K,V> mp;
		XmlParser parser("Parser");
		try
		{
			Document doc;
			parser.parse(serStr, doc);
			Element message = doc.getRootElement();
			if(message.getTagName()!="" && message.getTagName().find("multimap-")!=string::npos
					&& message.getChildElements().size()>0)
			{
				string maptype = message.getTagName();
				string keytype = maptype.substr(maptype.find("-")+1);
				if(keytype.find("-")==string::npos)
					return mp;
				string valtype = keytype.substr(keytype.find("-")+1);
				keytype = keytype.substr(0, keytype.find("-"));
				for (int var = 0; var < (int)message.getChildElements().size(); var++)
				{
					Element entry = message.getChildElements().at(var);
					if(entry.getTagName()=="entry")
					{
						Element key = entry.getElementByName("key");
						Element value = entry.getElementByName("value");
						if(key.getTagName()!="" && value.getTagName()!="" && key.getChildElements().size()>0
								&& value.getChildElements().size()>0) {
							K k = unserialize<K>(&key.getChildElements().at(0), keytype);
							V v = unserialize<K>(&value.getChildElements().at(0), valtype);
							mp[k] = v;
						} else if(key.getTagName()!="" && key.getChildElements().size()>0) {
							K k = unserialize<K>(&key.getChildElements().at(0), keytype);
							V v;
							mp[k] = v;
						}
					}
				}
			}
		} catch(const XmlParseException& str) {
			cout << str.getMessage() << endl;
		} catch(...) {
			cout << "XML Parse Error" << endl;
		}
		return mp;
	}
	template <class T> static T unserialize(const string& objXml, const string& appName = "")
	{
		XMLSerialize serialize;
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
	template <class T> static T unserialize(Element* element, const string& appName = "")
	{
		XMLSerialize serialize;
		T t;
		string className = getClassName(t);
		T* tp = (T*)_handleAllUnSerialization("",className,appName,&serialize,false,element);
		if(tp!=NULL)
		{
			t = *(T*)tp;
			delete ((T*)tp);
		}
		return t;
	}
	template <class T> static T unserialize(Element* element, const string& className, const string& appName = "")
	{
		XMLSerialize serialize;
		T t;
		T* tp = (T*)_handleAllUnSerialization("",className,appName,&serialize,false,element);
		if(tp!=NULL)
		{
			t = *(T*)tp;
			delete ((T*)tp);
		}
		return t;
	}
	template <class T> static T* unserializeToPointer(string objXml, const string& appName = "")
	{
		XMLSerialize serialize;
		T* t;
		string className = getClassName(t);
		return (T*)_handleAllUnSerialization(objXml,className,appName,&serialize,false,NULL);
	}
	template <class T> static T* unserializeToPointer(Element* element, const string& appName = "")
	{
		XMLSerialize serialize;
		T* t;
		string className = getClassName(t);
		return (T*)_handleAllUnSerialization("",className,appName,&serialize,false,element);
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

#endif /* XMLSERIALIZE_H_ */
