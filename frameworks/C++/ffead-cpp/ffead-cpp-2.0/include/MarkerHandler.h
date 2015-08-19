/*
 * MarkerHandler.h
 *
 *  Created on: 20-Aug-2014
 *      Author: sumeetc
 */

#ifndef MARKERHANDLER_H_
#define MARKERHANDLER_H_
#include "string"
#include "vector"
#include "map"
#include "StringUtil.h"
#include <stdarg.h>
#include <iostream>
using namespace std;

class MarkerHandler;

class Marker {
public:
	const map<string, bool>& getAttributes() const;
	string getAttributeValue(const string&);
	const string& getName() const;
	const bool isTypeClass() const;
	const bool isTypeProp() const;
	const bool isTypeMeth() const;
	const bool isTypeArg() const;
	static string getTypeName(const int&);
	string getTypeName();
	Marker();
private:
	int reqAttrSize;
	enum {TYPE_CLASS, TYPE_PROP, TYPE_METH, TYPE_ARG};
	string name;
	map<string, bool> attributes;
	map<string, vector<string> > valueSet;
	map<string, string> defValues;
	map<string, string> attributeValues;
	int type;
	Marker(const string& name, const int& type);
	Marker(const string& name, const int& type, const vector<string>& attributes);
	Marker(const string& name, const int& type, const vector<string>& attributes, const vector<bool>& reqLst);
	friend class MarkerHandler;
	friend class ConfigurationHandler;
};

class MarkerHandler {
	vector<Marker> validMarkers;
	void initMarkers();
	Marker getMarker(const string& name, const int& where);
	Marker getMarker(const string& name);
public:
	static vector<string> collectStr(int num, ...);
	static vector<bool> collectBool(int num, ...);
	void addMarker(const Marker& m);
	MarkerHandler();
	virtual ~MarkerHandler();
	Marker processMarker(string markerText, const int& where);
};

#endif /* MARKERHANDLER_H_ */
