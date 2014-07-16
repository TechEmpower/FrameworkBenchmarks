/*
 * main.cpp
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
#include <assert.h>
#include "include/socketd_internal.H"
#include "include/configparser.H"
#include <cpoll/cpoll.H>
#include <tuple>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <stdexcept>

using namespace RGC;
using namespace socketd;
using namespace std;
void listenthread();

tuple<const char*, int> mapFile(const char* path) {
	struct stat st;
	if (stat(path, &st) != 0) {
		if (errno == ENOENT) return make_tuple((const char*) NULL, 0);
		else throw runtime_error(strerror(errno));
	}
	if (st.st_size == 0) return make_tuple((const char*) 1, 0);
	int fd = open(path, O_RDONLY);
	if (fd < 0) throw runtime_error(strerror(errno));

	void* tmp = mmap(NULL, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
	if (tmp == NULL) throw runtime_error(strerror(errno));
	return make_tuple((const char*) tmp, st.st_size);
}
socketd::socketd sd;
#define PRINTSIZE(x) printf("sizeof("#x") = %i\n",sizeof(x))

int main(int argc, char** argv) {
	if (argc < 2) {
		printf("usage: %s socketd.conf\n", argv[0]);
		return 1;
	}
	{
		const char* confPath = argv[1];
		tuple<const char*, int> conf = mapFile(confPath);
		if (get < 0 > (conf) == NULL) {
			printf("config file not found: %s\n", strerror(errno));
			return 1;
		}
		loadConfig(get < 0 > (conf), get < 1 > (conf), sd);
		munmap((void*)get < 0 > (conf), get < 1 > (conf));
	}
	/*
	 sd.listens.push_back( { "0.0.0.0", "16969", 1, 32 });
	 sd.vhosts.push_back( { { { 0, "/asdf", "", binding::match_httpPath } }, "vhost1",
	 "/home/xaxaxa/workspace/test/socketd_test", "", true });
	 sd.vhosts.push_back( { { { 0, "/zxcv", "", binding::match_httpPath } }, "vhost2",
	 "/home/xaxaxa/workspace/test/socketd_test", "", false });
	 sd.vhosts.push_back( { { { 0, "/zzz", "", binding::match_httpPath } }, "vhost3",
	 "exec nc -l 0.0.0.0 12345 < /dev/urandom", "", true });
	 sd.vhosts.push_back( { { { 0, "/sss", "", binding::match_httpPath } }, "vhost4",
	 "lighttpd -D -f /home/xaxaxa/workspace/test/lighttpd.conf", "", true });*/
//sd.vhosts.push_back({{{1,"","",binding::match_listenID}},"vhost1","/home/xaxaxa/workspace/test/socketd_test",""});
	PRINTSIZE(socketd::socketd);
	PRINTSIZE(socketd::vhost);
	PRINTSIZE(socketd::listen);
	sd.run();
}
void listenthread() {
//CP::Poll p;

	/*config::rtconfigmanager *c=config::rtconfigmanager::getmainconfig();
	 SocketManager m;
	 int i;
	 for(i=0;i<c->listens_c;i++)
	 {
	 Socket s(AF_INET,SOCK_STREAM|SOCK_CLOEXEC,0);
	 s.Bind(c->listens[i].ep);
	 s.Listen(c->listens[i].backlog);
	 m.BeginAccept(s,SocketManager::Callback(cb1,NULL));
	 }
	 m.EventLoop();*/

}
