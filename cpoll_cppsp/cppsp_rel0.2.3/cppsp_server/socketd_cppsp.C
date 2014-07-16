/*
 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * */
#include <cpoll/cpoll.H>
#include <unistd.h>
#include <iostream>
#include <socketd.H>
#include <signal.h>
#include <socketd_client.H>
#include <cppsp/page.H>
#include <cppsp/cppsp_cpoll.H>
#include <cppsp/common.H>
#include "server.C"

using namespace std;
using namespace socketd;
using namespace CP;
using namespace cppsp;
string rootDir;
void parseArgs(int argc, char** argv, const function<void(char*, const function<char*()>&)>& cb) {
	int i = 1;
	function<char*()> func = [&]()->char*
	{
		if(i+1>=argc)return NULL;
		return argv[(++i)];
	};
	for (; i < argc; i++) {
		if (argv[i][0] == '\x00') continue;
		if (argv[i][0] == '-') {
			cb(argv[i] + 1, func);
		} else {
			cb(NULL, [argv,i]()
			{	return argv[i];});
		}
	}
}
int main(int argc, char** argv) {
	cout << "started child #" << getpid() << endl;
	srand(int(getpid())^(int)time(NULL));
	{
		char cwd[255];
		if(getcwd(cwd,255)==NULL) throw runtime_error(strerror(errno));
		rootDir=cwd;
	}
	CP::Poll p;
	vector<string> cxxopts;
	vector<const char*> modules;
	parseArgs(argc, argv,
			[&](char* name, const std::function<char*()>& getvalue)
			{
				if(strcmp(name,"r")==0) {
					rootDir=getvalue();
				} else if(strcmp(name,"c")==0) {
					cxxopts.push_back(getvalue());
				} else if(strcmp(name,"m")==0) {
					modules.push_back(getvalue());
				}
			});
	
	cppspServer::Server srv(rootDir.c_str());
	auto& v=CXXOpts(srv.mgr);
	v.insert(v.end(),cxxopts.begin(),cxxopts.end());
	cxxopts.clear();
	
	MemoryPool mp(sizeof(cppspServer::handler));
	struct {
		CP::Poll& p;
		MemoryPool& mp;
		cppspServer::Server& srv;
		void operator()(socketd_client& cl, Socket* s, int64_t id) {
			if(s==NULL)kill(getpid(),9);
			cl.ack(id);
			cppspServer::handler* hdlr=new (mp.alloc())
				cppspServer::handler(srv,p,*s);
			hdlr->allocator=&mp;
		}
	} cb {p, mp, srv};
	
	int modsLeft;
	struct {
		int& modsLeft;
		CP::Poll& p;
		Delegate<void(socketd_client& cl, Socket* s, int64_t id)> cb;
		void operator()() {
			if(--modsLeft == 0) {
				new socketd_client(p,cb);
			}
		}
	} afterModuleLoad {modsLeft,p,&cb};
	struct {
		const char* s;
		Delegate<void()> afterModuleLoad;
		void operator()(void*,exception* ex) {
			if(ex!=NULL) {
				fprintf(stderr,"error loading module %s: %s\n",s,ex->what());
				cppsp::CompileException* ce = dynamic_cast<cppsp::CompileException*>(ex);
				if (ce != NULL) {
					printf("%s\n",ce->compilerOutput.c_str());
				}
			}
			afterModuleLoad();
		}
	} moduleCB[modules.size()];
	modsLeft=modules.size();
	for(int ii=0;ii<(int)modules.size();ii++) {
		moduleCB[ii].s=modules[ii];
		moduleCB[ii].afterModuleLoad=&afterModuleLoad;
		srv.loadModule(p,modules[ii],&moduleCB[ii]);
	}
	if(modules.size()==0) new socketd_client(p,&cb);
	
	p.loop();
}
