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
 * ClassStructure.h
 *
 *  Created on: 14-Jun-2013
 *      Author: sumeetc
 */

#ifndef CLASSSTRUCTURE_H_
#define CLASSSTRUCTURE_H_
#include "vector"
#include "string"
#include "map"
#include "MarkerHandler.h"
using namespace std;

typedef vector<string> strVec;
typedef map<string, string> propMap;

class PropStructure
{
	string type;
	string name;
	string decl;
	map<string, vector<Marker> > markers;
	friend class Reflection;
	friend class ConfigurationHandler;
};

class MethStructure
{
	string name;
	map<int, string > argstypes;
	map<int, map<string, vector<Marker> > > argMarkers;
	string decl;
	map<string, vector<Marker> > markers;
	friend class Reflection;
	friend class ConfigurationHandler;
};

class ClassStructure
{
	map<string, vector<Marker> > markers;
	bool prosetser;
	strVec pub,pri,pro;
	vector<PropStructure> pubps, prips, props;
	vector<MethStructure> pubms, prims, proms;
	string classN,baseClassN,bcvisib,nmSpc;
	vector<string> namespaces;
	vector<PropStructure> getAllProps()
	{
		vector<PropStructure> a;
		a.insert(a.end(), prips.begin(), prips.end());
		a.insert(a.end(), props.begin(), props.end());
		a.insert(a.end(), pubps.begin(), pubps.end());
		return a;
	}
	friend class Reflection;
	friend class ConfigurationHandler;
public:
	string appName, incfile;
	string toString()
	{
		string out;
		string tab;
		for (int var = 0; var < (int)namespaces.size(); ++var) {
			out.append("using namespace " + namespaces.at(var) + "n");
		}
		vector<string> clnms = StringUtil::splitAndReturn<vector<string> >(nmSpc, "::");
		if(clnms.size()!=0)
		{
			for (int var = 0; var < (int)clnms.size(); ++var) {
				out.append(tab + "namespace " + clnms.at(var) + " {n");
				tab += "t";
			}
		}

		out.append(tab + "class " + classN);
		if(baseClassN!="")
		{
			out.append(" : " + bcvisib + " " + baseClassN + " {n");
		}
		for (int var = 0; var < (int)pri.size(); ++var) {
			out.append(tab + "t" + pri.at(var) + "n");
		}
		if(pro.size()>0)
		{
			out.append(tab + "protected:n");
		}
		for (int var = 0; var < (int)pro.size(); ++var) {
			out.append(tab + "t" + pro.at(var) + "n");
		}
		if(pub.size()>0)
		{
			out.append(tab + "public:n");
		}
		for (int var = 0; var < (int)pub.size(); ++var) {
			out.append(tab + "t" + pub.at(var) + "n");
		}

		out.append(tab + "}n");

		if(clnms.size()!=0)
		{
			for (int var = 0; var < (int)clnms.size(); ++var) {
				for (int va1r = 0; va1r < var; ++va1r) {
					out.append("t");
				}
				out.append("}n");
			}
		}
		return out;
	}
	string getTreatedClassName(const bool& flag)
	{
		if(flag)
		{
			string nm = nmSpc;
			StringUtil::replaceAll(nm, "::", "_");
			return nm+classN;
		}
		else
		{
			return classN;
		}
	}
	string getFullyQualifiedClassName()
	{
		/*if(nmSpc!="")
		{
			return nmSpc+classN;
		}*/
		return nmSpc+classN;
	}
	vector<string> getNamespaces()
	{
		return namespaces;
	}
	string getNamespace()
	{
		return nmSpc;
	}
};


#endif /* CLASSSTRUCTURE_H_ */
