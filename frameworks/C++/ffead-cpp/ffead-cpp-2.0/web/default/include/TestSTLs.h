/*
	Copyright 2009-2012, Sumeet Chhetri 
  
    Licensed under the Apache License, Version 2.0 (the "License"); 
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
 * TestSTLs.h
 *
 *  Created on: Jan 31, 2013
 *      Author: sumeet
 */

#ifndef TESTSTLS_H_
#define TESTSTLS_H_
#include "Test.h"
#include "CastUtil.h"
#include "vector"
#include "YObject.h"
#include "queue"
#include "deque"
#include "set"
#include "list"
class TestSTLs {
	list<int> vpli;
	vector<int> vpvi;
	set<int> vpls;
	multiset<int> vpmsi;
	std::queue<int> vpqi;
	deque<int> vpdi;

	list<int>* vppli;
	vector<int>* vppvi;
	set<int>* vppls;
	multiset<int>* vppmsi;
	std::queue<int>* vppqi;
	deque<int>* vppdi;
public:
	int mi;

	list<int> vli;
	list<string> vls;
	list<double> vld;
	list<long> vll;
	list<bool> vlb;
	list<short> vlsh;
	list<YObject> vlyo;

	vector<int> vvi;
	vector<string> vvs;
	vector<double> vvd;
	vector<long> vvl;
	vector<bool> vvb;
	vector<short> vvsh;
	vector<YObject> vvyo;

	deque<int> vdi;
	deque<string> vds;
	deque<double> vdd;
	deque<long> vdl;
	deque<bool> vdb;
	deque<short> vdsh;
	deque<YObject> vdyo;

	set<int> vsi;
	set<string> vss;
	set<double> vsd;
	set<long> vsl;
	set<short> vssh;
	set<Test> vsyo;

	multiset<int> vmsi;
	multiset<string> vmss;
	multiset<double> vmsd;
	multiset<long> vmsl;
	multiset<short> vmssh;
	multiset<Test> vmsyo;

	std::queue<int> vqi;
	std::queue<string> vqs;
	std::queue<double> vqd;
	std::queue<long> vql;
	std::queue<bool> vqb;
	std::queue<short> vqsh;
	std::queue<YObject> vqyo;

	list<int>* vpppli;
	list<string>* vpppls;
	list<double>* vpppld;
	list<long>* vpppll;
	list<bool>* vppplb;
	list<short>* vppplsh;
	list<YObject>* vppplyo;

	vector<int>* vpppvi;
	vector<string>* vpppvs;
	vector<double>* vpppvd;
	vector<long>* vpppvl;
	vector<bool>* vpppvb;
	vector<short>* vpppvsh;
	vector<YObject>* vpppvyo;

	deque<int>* vpppdi;
	deque<string>* vpppds;
	deque<double>* vpppdd;
	deque<long>* vpppdl;
	deque<bool>* vpppdb;
	deque<short>* vpppdsh;
	deque<YObject>* vpppdyo;

	set<int>* vpppsi;
	set<string>* vpppss;
	set<double>* vpppsd;
	set<long>* vpppsl;
	set<short>* vpppssh;
	set<Test>* vpppsyo;

	multiset<int>* vpppmsi;
	multiset<string>* vpppmss;
	multiset<double>* vpppmsd;
	multiset<long>* vpppmsl;
	multiset<short>* vpppmssh;
	multiset<Test>* vpppmsyo;

	std::queue<int>* vpppqi;
	std::queue<string>* vpppqs;
	std::queue<double>* vpppqd;
	std::queue<long>* vpppql;
	std::queue<bool>* vpppqb;
	std::queue<short>* vpppqsh;
	std::queue<YObject>* vpppqyo;

	YObject* yobjectp;

	YObject yobject;
public:
	deque<int> getVpdi() const
	{
		return vpdi;
	}

	void setVpdi(deque<int> vpdi)
	{
		this->vpdi = vpdi;
	}

	list<int> getVpli() const
	{
		return vpli;
	}

	void setVpli(list<int> vpli)
	{
		this->vpli = vpli;
	}

	set<int> getVpls() const
	{
		return vpls;
	}

	void setVpls(set<int> vpls)
	{
		this->vpls = vpls;
	}

	multiset<int> getVpmsi() const
	{
		return vpmsi;
	}

	void setVpmsi(multiset<int> vpmsi)
	{
		this->vpmsi = vpmsi;
	}

	std::queue<int> getVpqi() const
	{
		return vpqi;
	}

	void setVpqi(std::queue<int> vpqi)
	{
		this->vpqi = vpqi;
	}

	vector<int> getVpvi() const
	{
		return vpvi;
	}

	void setVpvi(vector<int> vpvi)
	{
		this->vpvi = vpvi;
	}

	deque<int>* getVppdi() const {
		return vppdi;
	}

	void setVppdi(deque<int>* vppdi) {
		this->vppdi = vppdi;
	}

	list<int>* getVppli() const {
		return vppli;
	}

	void setVppli(list<int>* vppli) {
		this->vppli = vppli;
	}

	set<int>* getVppls() const {
		return vppls;
	}

	void setVppls(set<int>* vppls) {
		this->vppls = vppls;
	}

	multiset<int>* getVppmsi() const {
		return vppmsi;
	}

	void setVppmsi(multiset<int>* vppmsi) {
		this->vppmsi = vppmsi;
	}

	std::queue<int>* getVppqi() const {
		return vppqi;
	}

	void setVppqi(std::queue<int>* vppqi) {
		this->vppqi = vppqi;
	}

	vector<int>* getVppvi() const {
		return vppvi;
	}

	void setVppvi(vector<int>* vppvi) {
		this->vppvi = vppvi;
	}

	bool operator <(TestSTLs t) const
	{
		return this->mi < t.mi;
	}

	TestSTLs()
	{
	}

	virtual ~TestSTLs()
	{
	}

	string toString()
	{
		string str;
		for (int var = 0; var < (int)vvi.size(); ++var) {
			str += CastUtil::lexical_cast<string>(vvi.at(var));
		}
		for (int var = 0; var < (int)vvsh.size(); ++var) {
			str += CastUtil::lexical_cast<string>(vvsh.at(var));
		}
		for (int var = 0; var < (int)vvl.size(); ++var) {
			str += CastUtil::lexical_cast<string>(vvl.at(var));
		}
		for (int var = 0; var < (int)vvd.size(); ++var) {
			str += CastUtil::lexical_cast<string>(vvd.at(var));
		}
		for (int var = 0; var < (int)vvb.size(); ++var) {
			str += CastUtil::lexical_cast<string>(vvb.at(var));
		}
		for (int var = 0; var < (int)vvs.size(); ++var) {
			str += CastUtil::lexical_cast<string>(vvs.at(var));
		}

		for (list<int>::iterator it=vli.begin();it!=vli.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (list<short>::iterator it=vlsh.begin();it!=vlsh.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (list<long>::iterator it=vll.begin();it!=vll.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (list<double>::iterator it=vld.begin();it!=vld.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (list<bool>::iterator it=vlb.begin();it!=vlb.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (list<string>::iterator it=vls.begin();it!=vls.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}

		for (set<int>::iterator it=vsi.begin();it!=vsi.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (set<short>::iterator it=vssh.begin();it!=vssh.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (set<long>::iterator it=vsl.begin();it!=vsl.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (set<double>::iterator it=vsd.begin();it!=vsd.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (set<string>::iterator it=vss.begin();it!=vss.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}

		for (multiset<int>::iterator it=vmsi.begin();it!=vmsi.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (multiset<short>::iterator it=vmssh.begin();it!=vmssh.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (multiset<long>::iterator it=vmsl.begin();it!=vmsl.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (multiset<double>::iterator it=vmsd.begin();it!=vmsd.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (multiset<string>::iterator it=vmss.begin();it!=vmss.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}

		for (deque<int>::iterator it=vdi.begin();it!=vdi.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (deque<short>::iterator it=vdsh.begin();it!=vdsh.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (deque<long>::iterator it=vdl.begin();it!=vdl.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (deque<double>::iterator it=vdd.begin();it!=vdd.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (deque<bool>::iterator it=vdb.begin();it!=vdb.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}
		for (deque<string>::iterator it=vds.begin();it!=vds.end();it++) {
			str += CastUtil::lexical_cast<string>(*it);
		}

		std::queue<int> temp = vqi;
		while (!temp.empty()) {
			str += CastUtil::lexical_cast<string>(temp.front());
			temp.pop();
		}
		std::queue<short> tempsh = vqsh;
		while (!tempsh.empty()) {
			str += CastUtil::lexical_cast<string>(tempsh.front());
			tempsh.pop();
		}
		std::queue<long> templ = vql;
		while (!templ.empty()) {
			str += CastUtil::lexical_cast<string>(templ.front());
			templ.pop();
		}
		std::queue<double> tempd = vqd;
		while (!tempd.empty()) {
			str += CastUtil::lexical_cast<string>(tempd.front());
			tempd.pop();
		}
		std::queue<bool> tempb = vqb;
		while (!tempb.empty()) {
			str += CastUtil::lexical_cast<string>(tempb.front());
			tempb.pop();
		}
		std::queue<string> temps = vqs;
		while (!temps.empty()) {
			str += CastUtil::lexical_cast<string>(temps.front());
			temps.pop();
		}
		return str;
	}
};

#endif /* TESTSTLS_H_ */
