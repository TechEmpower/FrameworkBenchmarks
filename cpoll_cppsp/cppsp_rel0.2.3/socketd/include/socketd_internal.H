/*
 * config.hpp
 *
 *  Created on: 2011-05-20
 *      Author: xaxaxa
 */
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
#ifndef CONFIG_HPP_
#define CONFIG_HPP_

#include <cpoll/cpoll.H>
#include <rgc.H>
#include <string>
#include <map>
#include <vector>
#include "socketd.H"
#include <delegate.H>
#include <unistd.h>

using namespace std;

namespace socketd
{
	class SocketDException: public std::exception
	{
	public:
		string message;
		int32_t number;
		SocketDException();
		SocketDException(int32_t number);
		SocketDException(string message, int32_t number = 0);
		~SocketDException() throw ();
		const char* what() const throw ();
	};

	struct binding;
	struct vhost;
	struct listen
	{
	public:
		//user supplied fields
		//RGC::Ref<CP::EndPoint> ep;
		string host;
		string port;
		int id;
		int backlog;
		//internal fields
		vector<CP::HANDLE> socks;
		int d,t,p;
		listen(string host, string port, int id, int backlog = 32) :
				host(host), port(port), id(id), backlog(backlog) {
		}
		listen() :
				backlog(32) {
		}
	};
	struct binding
	{
	public:
		//user supplied fields
		string httpPath; //path must not have / at the end!!!!!
		string httpHost;
		string vhostName; //only needs to be filled if put in the "extraBindings" array
		int listenID;

		//bitmap:
		//0: match listenID
		//1: match httpPath
		//2: match httpHost
		int matchLevel;
		enum
		{
			match_listenID = 1, match_httpPath = 2, match_httpHost = 4
		};
		binding() {
		}
		binding(int listenID, string httpPath, string httpHost, int matchLevel) :
				httpPath(httpPath), httpHost(httpHost), listenID(listenID), matchLevel(matchLevel) {
		}
		//internal fields
		vhost* vh;
	};
	struct appConnection: public RGC::Object
	{
		//						sock		success
		typedef Delegate<void(bool)> passConnCB;
		appConnection();
		virtual void shutDown()=0;

		//return values: 0: success; 1: failed; 2: in progress (passConnCB() will be called later)
		virtual int passConnection(CP::Socket* s, void* buffer, int buflen, const passConnCB& cb)=0;
		virtual ~appConnection();
	};
	struct vhost: public RGC::Object
	{
	public:
		//user supplied fields
		vector<binding> bindings;
		string name;
		string exepath; //leave blank (length==0) to disable execing and just wait for attachment

		/*int requestsPerProcess;
		 int maxRequestsPerProcess;
		 int maxProcesses;*/
		int processes; //how many processes to spawn (for load balancing)
		//at runtime, the # of processes will be rounded up to the nearest multiple
		//of the # of socketd threads

		int ipcBufSize; //set to 0 to use system default, -1 to use global settings
		//for attaching to a running vhost; arbitrary string; leave blank (length==0) to
		//disable attachments
		string authCookie;

		//whether to LD_PRELOAD socketd_proxy.so
		bool preload;
		bool useShell;
		vhost() :
				processes(1), ipcBufSize(-1), preload(false), useShell(true) {
		}
		vhost(const vector<binding>& bindings, string name, string exepath, string authCookie,
				bool preload = false, bool shell = true, int processes = 1) :
				bindings(bindings), name(name), exepath(exepath), processes(processes),
						authCookie(authCookie), preload(preload), useShell(shell) {
		}

		//internal fields
		//vector<int> conns_i; //indexed by thread*processes + curProcess
		RGC::Ref<appConnection> attachmentConn; //readonly by threads
		//vector<int> curProcess_i;
		int _ipcBufSize;
		int _processes; //processes per thread
		vector<uint8_t*> perCPUData;
		bool hasAttachments;
	};
	static const char* socketd_proxy = "socketd_proxy.so";
	class socketd
	{
	public:
		//user supplied fields
		vector<listen> listens;
		vector<vhost> vhosts;
		vector<binding> extraBindings; //vhostName must be filled for these
		string unixAddress;
		uint8_t** perCPUData;
		//format of per-cpu data structures:
		//struct {
		//		appConnection* conns[processes];
		//		int curProcess;
		//		int padding;
		//} vhostInfo[vhosts];
		int ipcBufSize; //<=0 to use system default
		int threads;
		void run();

		//internal fields
		vector<binding*> bindings;
		socketd() :
				ipcBufSize(0), threads((int) sysconf(_SC_NPROCESSORS_CONF)) {
		}
	};

}

#endif /* CONFIG_HPP_ */
