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

#ifndef AMEFOBJECT_H_
#define AMEFOBJECT_H_

#include "string"
#include "cstring"
#include "vector"
#include "CastUtil.h"
using namespace std;

class AMEFObject
{
	/*The type of the Object can be string, number, date, boolean, character or any complex object*/
	char type;

	/*The name of the Object if required can be used to represent object properties*/
	string name;

	/*The Object value in string format*/
	string value;

	int position;

	int blength;

	/*The properties of a complex object*/
	vector<AMEFObject*> packets;

	AMEFObject* addStringPacket(const string& stringa);
	AMEFObject* addCharStringPacket(char* stringa);
	AMEFObject* addBoolPacket(const bool& boole);
	AMEFObject* addCharPacket(const char& chr);
	AMEFObject* addULLPacket(const unsigned long long& lon);
	AMEFObject* addFloatPacket(const float& doub);
	AMEFObject* addDoublePacket(const double& doub);
	AMEFObject* addLDoublePacket(const long double& doub);
	friend class CacheMap;
	friend class AMEFDecoder;
public:
	static int charArrayToInt(const string& l, const int& off, const int& ind);
	static string intTocharArray(const int& l, const int& ind);
	static string ulonglongTocharArray(const unsigned long long& lon, int ind = -1);
	static unsigned long long charArrayToULongLong(const string& l);
	static unsigned long long charArrayToULongLong(const string& l, const int&);
	static string ulonglongTocharArrayWithLI(const unsigned long long& lon);
	static void ulonglongTocharArrayF(const unsigned long long& lon, string& result, int ind = -1)
	{
		if(ind==-1)
		{
			if(lon<4294967296ULL)
			{
				ind = 4;
			}
			else
			{
				ind = 8;
			}
		}
		for (int i = 0; i<ind; i++)
		{
			int offset = (ind - 1 - i) * 8;
			result.push_back((char) ((lon >> offset) & 0xFF));
		}
	}

	static const char NULL_STRING = 'a';

	static const char NULL_NUMBER = 'b';

	static const char NULL_DATE = 'c';

	static const char NULL_FPN = 'd';

	static const char NULL_BOOL = 'e';

	static const char NULL_CHAR = 'f';

	static const char NULL_OBJECT = 'g';

	static const char INT_TYPE = 'h';

	static const char LONG_INT_TYPE = 'i';

	static const char BOOLEAN_TYPE = 'j';

	static const char CHAR_TYPE = 'k';

	static const char DATE_TYPE = 'l';

	static const char DOUBLE_FLOAT_TYPE = 'm';

	static const char ASCII_STRING_TYPE = 'n';

	static const char STRING_TYPE = 'o';

	static const char OBJECT_TYPE = 'p';

	/*Create a new AMEF object which will initilaize the values*/
	AMEFObject();

	~AMEFObject();

	bool isNull();

	void addNullPacket(const char& type, const string& name="");

	void addPacket(const string& stringa, const string name= "");

	void addPacket(char* stringa, const string name= "");

	void addPacket(const bool& boole, const string name= "");

	void addPacket(const char& chr, const string name= "");

	void addPacket(const unsigned char& chr, const string name= "");

	void addPacket(const short& integer, const string name= "");

	void addPacket(const unsigned short& integer, const string name= "");

	void addPacket(const int& integer, const string name= "");

	void addPacket(const unsigned int& integer, const string name= "");

	void addPacket(const long& lon, const string name= "");

	void addPacket(const unsigned long& lon, const string name= "");

	void addPacket(const long long& lon, const string name= "");

	void addPacket(const unsigned long long& lon, const string name= "");

	void addPacket(const float& doub, const string name= "");

	void addPacket(const double& doub, const string name= "");

	void addPacket(const long double& doub, const string name= "");

	void addPacket(AMEFObject *packet);

	static char getEqvNullType(const char& type);

	void clear();

	static bool isFloatingPoint(const char& type);

	static bool isNumber(const char& type);

	static bool isInteger(const char& type);

	static bool isLong(const char& type);

	static bool isChar(const char& type);

	static bool isBoolean(const char& type);

	bool isBoolean();

	static bool isDate(const char& type);

	bool  isStringOrNullString();

	bool  isString();

	bool  isFloatingPoint();

	bool  isNumber();

	bool  isNumberOrNullNumber();

	bool  isChar();

	bool  isDate();

	char* getName();

	string getNameStr();

	void setName(const string& name);

	vector<AMEFObject*> getPackets();

	void setPackets(vector<AMEFObject*> packets);

	char getType();

	void setType(const char& type);

	string getValue();

	string getValueStr();

	void pushChar(const char& v);

	void setValue(unsigned long long lon);

	void setValue(char* value);

	void setValue(char *value, const int& len);

	void setValue(const string& value);

	char getCharValue();

	unsigned char getUCharValue();

	bool getBoolValue();

	int getIntValue();

	unsigned int getUIntValue();

	short getShortValue();

	unsigned short getUShortValue();

	long getLongValue();

	unsigned long getULongValue();

	unsigned long long getNumericValue();

	long long getLongLongValue();

	unsigned long long getULongLongValue();

	double getDoubleValue();

	long double getLongDoubleValue();

	float getFloatValue();

	string tostring();

	string displayObject(const string& tab);

	void addStaticPacket(AMEFObject *obj);

	/*template<class T> T getObjectValue()
	{
		T t;

		string desiredType = CastUtil::getTypeName<T>();
		if(!CastUtil::isPrimitiveDataType<T>())
		{
			throw "Invalid Object value requested";
		}

		if(desiredType=="short" || desiredType=="short int" || desiredType=="signed short" || desiredType=="signed short int"
				|| desiredType=="unsigned short" || desiredType=="unsigned short int"
				|| desiredType=="signed" || desiredType=="int" || desiredType=="signed int"
				|| desiredType=="unsigned" || desiredType=="unsigned int" || desiredType=="long"
				|| desiredType=="long int" || desiredType=="signed long" || desiredType=="signed long int"
				|| desiredType=="unsigned long" || desiredType=="unsigned long int"
				|| desiredType=="long long" || desiredType=="long long int" || desiredType=="signed long long"
				|| desiredType=="signed long long int" || desiredType=="unsigned long long"
				|| desiredType=="unsigned long long int")
		{
			if((type=='f' || type=='x' || type=='e' || type=='l'
						|| type=='n' || type=='w' || type=='r' || type=='i') && value.length()<=8)
			{
			}
			else
			{
				throw "Invalid Object value requested";
			}
			if(desiredType=="short")
			{
				short v = (short)charArrayToULongLong(value);
				t = v;
			}
			else if(desiredType=="unsigned short")
			{
				unsigned short v = (unsigned short)charArrayToULongLong(value);
				t = v;
			}
			else if(desiredType=="int")
			{
				int v = (int)charArrayToULongLong(value);
				t = v;
			}
			else if(desiredType=="unsigned int")
			{
				unsigned int v = (unsigned int)charArrayToULongLong(value);
				t = v;
			}
			else if(desiredType=="long")
			{
				long v = (long)charArrayToULongLong(value);
				t = v;
			}
			else if(desiredType=="unsigned long")
			{
				unsigned long v = (unsigned long)charArrayToULongLong(value);
				t = v;
			}
			else if(desiredType=="long long")
			{
				long long v = (long long)charArrayToULongLong(value);
				t = v;
			}
			else if(desiredType=="unsigned long long")
			{
				unsigned long long v = (unsigned long long)charArrayToULongLong(value);
				t = v;
			}
		}
		else if(desiredType=="long double" || desiredType=="float" || desiredType=="double"
				|| desiredType=="string" || desiredType=="std::string")
		{
			t = CastUtil::lexical_cast<T>(getValue());
		}
		else if(desiredType=="bool")
		{
			bool v = getBoolValue();
			t = v;
		}
		return t;
	}*/
};


#endif /* AMEFOBJECT_H_ */
