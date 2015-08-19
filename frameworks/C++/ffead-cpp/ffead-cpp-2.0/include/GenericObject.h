/*
 * GenericObject.h
 *
 *  Created on: 09-Oct-2014
 *      Author: sumeetc
 */

#ifndef GENERICOBJECTTYPE_H_
#define GENERICOBJECTTYPE_H_
#include <cxxabi.h>
#include <typeinfo>
#include <wchar.h>
#include "XMLSerialize.h"
using namespace std;

class GenericObject {
	static map<string, string> _mangledClassNameMap;
	string typeName;
	string objSerState;
	void* objVal;
	vector<char> cstr;
	vector<unsigned char> ucstr;
	vector<wchar_t> wstr;
	void internalClear();
	void internalCopy(const GenericObject &obj);
public:
	GenericObject();
	GenericObject& operator = (const GenericObject &obj);
	GenericObject(const GenericObject &obj);
	~GenericObject();
	const string& getTypeName() const;
	bool isInstanceOf(const string&);
	bool isNumber();
	bool isBool();
	bool isFPN();
	bool isString();
	bool isObject();
	bool isNull();
	string getSerilaizedState();
	void* getPointer();

	static bool isNumber(const string& typeName);
	static bool isNumber32(const string& typeName);
	static bool isNumber64(const string& typeName);
	static bool isString(const string& typeName);
	static bool isFPN(const string& typeName);
	static bool isPrimitive(const string& typeName);
	template <typename T> static T getObjectFromSerilaizedState(const string& serilaizedState);

	template <typename T> void operator<<(T &t)
	{
		set(t);
	}

	template <typename T> void operator<<(T *t)
	{
		set(t);
	}

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

	template<class T> void set(T* t, const size_t& strlength= string::npos) {
		string typeName = getClassName(t);
		set((void*)t, typeName, strlength);
	}

	void set(void* t, string typeName, size_t strlength= string::npos) {
		if(typeName.at(typeName.length()-1)=='*')
		{
			typeName = typeName.substr(0, typeName.length()-1);
		}
		if(typeName.at(typeName.length()-1)=='*')
		{
			throw "Cannot handle double pointers and beyond...";
		}
		if(typeName.find(",")!=string::npos)
		{
			typeName = typeName.substr(0, typeName.find(",")+1);
		}
		this->typeName = typeName;
		internalClear();
		if(typeName=="short") objVal = new short(*(short*)t);
		else if(typeName=="int") objVal = new int(*(int*)t);
		else if(typeName=="long") objVal = new long(*(long*)t);
		else if(typeName=="long long") objVal = new long long(*(long long*)t);
		else if(typeName=="unsigned short") objVal = new unsigned short(*(unsigned short*)t);
		else if(typeName=="unsigned int") objVal = new unsigned int(*(unsigned int*)t);
		else if(typeName=="unsigned long") objVal = new unsigned long(*(unsigned long*)t);
		else if(typeName=="unsigned long long") objVal = new unsigned long long(*(unsigned long long*)t);
		else if(typeName=="bool") objVal = new bool(*(bool*)t);
		else if(typeName=="float") objVal = new float(*(float*)t);
		else if(typeName=="double") objVal = new double(*(double*)t);
		else if(typeName=="long double") objVal = new long double(*(long double*)t);
		else if(typeName=="std::string" || typeName=="string") objVal = new string(*(string*)t);
		else if(typeName=="char" || typeName=="char const") {
			char* src = (char*)t;
			if(src) {
				if(strlength==string::npos) {
					strlength = strlen(src);
				}
				cstr.assign(src, src+strlength);
				char* cstrptr = new char[cstr.size()];
				std::copy(cstr.begin(), cstr.end(), cstrptr);
				objVal = cstrptr;
			}
		}
		else if(typeName=="unsigned char" || typeName=="unsigned char const") {
			unsigned char* src = (unsigned char*)t;
			if(src) {
				if(strlength==string::npos) {
					strlength = strlen((char*)src);
				}
				ucstr.assign(src, src+strlength);
				unsigned char* ucstrpr = new unsigned char[ucstr.size()];
				std::copy(ucstr.begin(), ucstr.end(), ucstrpr);
				objVal = ucstrpr;
			}
		}
		else if(typeName=="wchar_t" || typeName=="wchar_t const") {
			wchar_t* src = (wchar_t*)t;
			if(src) {
				if(strlength==string::npos) {
					strlength = wcslen(src);
				}
				wstr.assign(src, src+strlength);
				wchar_t* wstrpr = new wchar_t[wstr.size()];
				std::copy(wstr.begin(), wstr.end(), wstrpr);
				objVal = wstrpr;
			}
		}
		//This mean this is some other object, try to serialize it...
		else
		{
			objSerState = XMLSerialize::serializeUnknown(t, typeName);
			objVal = XMLSerialize::unSerializeUnknown(objSerState, typeName);
		}
	}

	template<class T> void set(T t, const size_t& strlength= -1) {
		string typeName = getClassName(t);
		if(typeName.at(typeName.length()-1)=='*')
		{
			throw "Cannot handle pointer types use 'set(T* t)' instead...";
		}
		this->typeName = typeName;
		internalClear();
		if(typeName=="short") objVal = new short(*(short*)&t);
		else if(typeName=="int") objVal = new int(*(int*)&t);
		else if(typeName=="long") objVal = new long(*(long*)&t);
		else if(typeName=="long long") objVal = new long long(*(long long*)&t);
		else if(typeName=="unsigned short") objVal = new unsigned short(*(unsigned short*)&t);
		else if(typeName=="unsigned int") objVal = new unsigned int(*(unsigned int*)&t);
		else if(typeName=="unsigned long") objVal = new unsigned long(*(unsigned long*)&t);
		else if(typeName=="unsigned long long") objVal = new unsigned long long(*(unsigned long long*)&t);
		else if(typeName=="bool") objVal = new bool(*(bool*)&t);
		else if(typeName=="float") objVal = new float(*(float*)&t);
		else if(typeName=="double") objVal = new double(*(double*)&t);
		else if(typeName=="long double") objVal = new long double(*(long double*)&t);
		else if(typeName=="std::string" || typeName=="string") objVal = new string(*(string*)&t);
		//This means this is some other object, try to serialize it...
		else
		{
			objSerState = XMLSerialize::serialize<T>(t);
			objVal = XMLSerialize::unserializeToPointer<T>(objSerState);
		}
	}

	template<class T> void get(T*& t) {
		string typeName = getClassName(t);
		if(typeName.at(typeName.length()-1)=='*')
		{
			typeName = typeName.substr(0, typeName.length()-1);
		}
		if(typeName.at(typeName.length()-1)=='*')
		{
			throw "Cannot handle double pointers and beyond...";
		}

		t = (T*)objVal;
	}

	template<class T> void get(T& t) {
		string typeName = getClassName(t);
		if(typeName.at(typeName.length()-1)=='*')
		{
			throw "Cannot handle pointer types use 'getP()' instead...";
		}
		t = *(T*)objVal;
	}
};

template<typename T>
inline T GenericObject::getObjectFromSerilaizedState(const string& serilaizedState) {
	T t;
	string typeName = getClassName(t);
	if(typeName.at(typeName.length()-1)=='*')
	{
		throw "Cannot handle pointer types use 'getP()' instead...";
	}
	if(GenericObject::isPrimitive(typeName)) t = CastUtil::lexical_cast<T>(serilaizedState);
	else t = XMLSerialize::unserialize<T>(serilaizedState);
	return t;
}

#endif /* GENERICOBJECTTYPE_H_ */
