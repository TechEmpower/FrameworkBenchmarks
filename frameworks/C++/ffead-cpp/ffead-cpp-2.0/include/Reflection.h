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
/*
 * Reflection.h
 *
 *  Created on: Aug 21, 2009
 *      Author: sumeet
 */

#ifndef REFLECTION_H_
#define REFLECTION_H_
#include "CastUtil.h"
#include "LoggerFactory.h"
#include "RegexUtil.h"
#include "iostream"
#include "fstream"
#include "sstream"
#include "map"
#include "vector"
#include "set"
#include <stdio.h>
#include "StringUtil.h"
#include "ClassStructure.h"
#include "TemplateEngine.h"
#include "MarkerHandler.h"
#include "CommonUtils.h"

using namespace std;

/*class ClassStructure
{
	bool prosetser;
	strVec pub,pri,pro;
	string classN,baseClassN,bcvisib,nmSpc;
	vector<string> namespaces;
	friend class Reflection;
public:
	string appName, incfile;
	string toString()
	{
		string out;
		string tab;
		for (int var = 0; var < (int)namespaces.size(); ++var) {
			out.append("using namespace " + namespaces.at(var) + "n");
		}
		vector<string> clnms = StringUtil::split(nmSpc, "::");
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
		if(nmSpc!="")
		{
			return nmSpc+"::"+classN;
		}
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
};*/

class Reflection {
	//bool generateClassInfoFromDD(const string&);
	//bool generateClassInfo(const string&);
	//void collectInfo(const string&, const string&);
	map<string,bool> methsall;
	static map<string,bool> validcls;
	map<string,string> clspaths;
	map<string,string> classNamespaces;
	static map<string,int> nmspcIds;
	static map<string,string> nmspcIdVals;
	Logger logger;
	MarkerHandler handler;
	void handleNamespace(string data, string namepsc, map<string, ClassStructure>& clsvec, map<string, vector<string> >& glbnmspcs, vector<string> pragmas);
	int findless(const int& a, const int& b, const int& c);
	void collectInfo(string data, const string& flag, ClassStructure& cls, const vector<string>& mrktxt);
	void emptyBlocks(string& data, size_t);
public:
	map<string, ClassStructure> getClassStructures(const string& className, const string&);
	//strVec list(const string&);
	static bool isValidClass(const string& claz, const string& app)
	{
		return validcls.find(app+claz)!=validcls.end();
	}
	string getClassPath(const string& claz)
	{
		if(clspaths.find(claz)!=clspaths.end())
		{
			return clspaths[claz];
		}
		return "";
	}
	static string getNameSpaceId(const string& nmsp, const string& app)
	{
		if(nmspcIds.find(app+nmsp)!=nmspcIds.end())
		{
			return "ns" + CastUtil::lexical_cast<string>(nmspcIds[app+nmsp]);
		}
		return "";
	}
	static string getNameSpaceIdValue(const string& nmspid)
	{
		if(nmspcIdVals.find(nmspid)!=nmspcIdVals.end())
		{
			return nmspcIdVals[nmspid];
		}
		return "";
	}
	/*string getWsTreatedClassName()
	{
		if(nmSpc!="" && nmspcIds.find(nmSpc)!=nmspcIds.end())
		{
			return "ns" + CastUtil::lexical_cast<string>(nmspcIds[nmSpc]) + ":" + classN;
		}
		return classN;
	}*/
	string getFullyQualifiedClassName(const string& classN, const vector<string>& namespaces)
	{
		for (int var = 0; var < (int)namespaces.size(); ++var) {
			string tempfqnmclz = namespaces.at(var)+classN;
			if(classNamespaces.find(tempfqnmclz)!=classNamespaces.end())
			{
				return tempfqnmclz;
			}
		}
		return classN;
	}
	string getTreatedFullyQualifiedClassName(const string& classN, const vector<string>& namespaces, const bool& stripns= true)
	{
		for (int var = 0; var < (int)namespaces.size(); ++var) {
			string tempfqnmclz = namespaces.at(var)+classN;
			if(classNamespaces.find(tempfqnmclz)!=classNamespaces.end())
			{
				if(stripns)
				{
					StringUtil::replaceAll(tempfqnmclz, "::", "");
				}
				return tempfqnmclz;
			}
		}
		return classN;
	}
	Reflection();
	virtual ~Reflection();
	string getXSDDefinitions(map<string, ClassStructure>& allclsmap, const string& fqcn, Reflection& ref, const string& appname, string &trgnmspc, set<string> &allnmspcs, const string& dfnmspc, const string& resp);
	strVec getAfcObjectData(ClassStructure& classStructure, const bool& object, vector<string>& privf, bool &isOpForSet);
	//strVec getAfcObjectData(const string&, const bool&);
	propMap getDbTableInfo(const string&);
	string generateClassDefinitionsAll(map<string, map<string, ClassStructure> >& clsstrucMaps, string &includeRef, const vector<string>& apps);
	string generateSerDefinitionAll(map<string, map<string, ClassStructure> >& clsstrucMaps, string &includeRef, const bool& isBinary, string& objs, string& ajaxret, string& headers, string& typerefs, const vector<string>& apps);
	string generateClassDefinition(map<string, ClassStructure>& allclsmap, string &includesDefs, string &typedefs, string &classes, string &methods, string &opers, const string& app);
	string generateClassDefinition_Old(map<string, ClassStructure>& allclsmap, string &includesDefs, string &typedefs, string &classes, string &methods, string &opers, const string& app);
	string generateSerDefinition(map<string, ClassStructure>& allclsmap, string &includesDefs, string &typedefs, string &classes, string &methods, const string& app);
	string generateClassDefinitions(map<string, ClassStructure>& allclsmap, string &includesDefs, string &typedefs, string &classes, string &methods, string &opers, const string& app);
	string generateSerDefinitions(map<string, ClassStructure>& allclsmap, string &includesDefs, string &typedefs, string &classes, string &methods, const bool& isBinary, string& objs, string &ajaxret, string& headers, string& typerefs, const string& app);
	string generateSerDefinitionBinary(map<string, ClassStructure>& allclsmap, string &includesDefs, string &typedefs, string &classes, string &methods, const string& app);
	string generateAllSerDefinition(map<string, ClassStructure>& allclsmap, string &includesDefs, string &typedefs, string &classes, string &methods, const string& app);
	static string getTypeName(string type);
	static bool isPrimitiveDataType(string type);
	void addMarker(const Marker& m);
};

#endif /* REFLECTION_H_ */
