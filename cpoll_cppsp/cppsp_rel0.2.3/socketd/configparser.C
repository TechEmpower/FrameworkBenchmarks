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
/*
 * configparser.C
 *
 *  Created on: Mar 2, 2013
 *      Author: xaxaxa
 */
#include "include/configparser.H"
#include <functional>
#include <sstream>
#define CONCAT(s) (((stringstream&)(stringstream() << s)).str().c_str())
using namespace std;
namespace socketd
{
	struct configToken
	{
		enum types
		{
			none = 0, t_line, t_beginBlock, t_endBlock
		} type;
		const char* data;
		int datalen;
		struct parserInfo
		{
			int pos;
			int line;
		} inf;
	};
	ParserException::ParserException() :
			message(strerror(errno)), number(errno) {
	}
	ParserException::ParserException(int32_t number) :
			message(strerror(number)), number(number) {
	}
	ParserException::ParserException(string message, int32_t number) :
			message(message), number(number) {
	}
	ParserException::~ParserException() throw () {
	}
	const char* ParserException::what() const throw () {
		return message.c_str();
	}

	class ParserException_internal: public ParserException
	{
	public:
		string message;
		int32_t number;
		ParserException_internal() {
		}
		ParserException_internal(int32_t number) :
				ParserException(number) {
		}
		ParserException_internal(string message, int32_t number = 0) :
				ParserException(message, number) {
		}
		ParserException_internal(const configToken::parserInfo& inf, string message, int32_t number =
				0) :
				ParserException(CONCAT("line " << inf.line << ": " <<message), number) {

		}
		ParserException_internal(const configToken& ct, string message, int32_t number = 0) :
				ParserException(CONCAT("line " << ct.inf.line << ": " <<message), number) {

		}
		~ParserException_internal() throw () {
		}
	};

	inline bool isWhitespace(char ch) {
		return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r';
	}
	static inline void parseConfig_out(configToken& t, const char* conf, int len, int i, int& last_i,
			const function<void(const configToken&)>& cb) {
		while (last_i < i && isWhitespace(conf[last_i]))
			last_i++;
		t.data = conf + last_i;
		t.datalen = i - last_i;
		cb(t);
		last_i = i + 1;
	}
	void parseConfig(const char* conf, int len, const function<void(const configToken&)>& cb) {
		configToken t;
		int i = 0;
		int last_i = 0;
		int line = 1;
		//int tmpline = 0;
		main_loop: while (i < len) {
			if (conf[i] == '{') {
				t.type = configToken::t_beginBlock;
				goto out;
			} else if (conf[i] == '}') {
				for (int x = last_i; x < i; x++) {
					if (!isWhitespace(conf[x])) throw ParserException_internal( { last_i, line },
							"garbage before \"}\". maybe you forgot a \";\"?");
				}
				t.type = configToken::t_endBlock;
				goto out;
			} else if (conf[i] == ';') {
				t.type = configToken::t_line;
				goto out;
			} else if (conf[i] == '/') {
				goto loop2;
			} else if (conf[i] == '\n') {
				line++;
			}
			cont: i++;
		}
		return;
		loop2: i++;
		if (i >= len || conf[i] != '/') goto main_loop;
		i++;
		while (i < len) {
			if (conf[i] == '\r' || conf[i] == '\n') break;
			i++;
		}
		last_i = i;
		goto main_loop;

		out: t.inf.pos = last_i;
		t.inf.line = line;
		parseConfig_out(t, conf, len, i, last_i, cb);
		//line += tmpline;
		//tmpline = 0;
		goto cont;
	}

	//returns length of prefix, because index is always 0
	static inline int configGetPrefix(const char* data, int len) {
		const char* tmp = (const char*) memchr(data, ' ', len);
		if (tmp == NULL) return len;
		else return tmp - data;
	}
	static inline int mystrcmp(const char* s1, int l1, const char* s2, int l2) {
		if (l1 != l2) return l1 - l2;
		return memcmp(s1, s2, l1);
	}
	static inline void split(const char* s1, int l1, char c,
			const function<void(const char*, int)>& cb) {
		int i = 0;
		while (i < l1) {
			const void* tmp = memchr(s1 + i, c, l1 - i);
			if (tmp == NULL) break;
			int next = ((const char*) tmp) - s1;
			cb(s1 + i, next - i);
			i = next + 1;
		}
		cb(s1 + i, l1 - i);
	}
	static void parseBindingDirective(binding& b, socketd& sd, const configToken& ct) {
		int prefLen = configGetPrefix(ct.data, ct.datalen);
		if (ct.datalen - prefLen - 1 <= 0) throw ParserException_internal(ct,
				"missing parameter in \"" + string(ct.data, prefLen) + "\" binding target");
		if (mystrcmp(ct.data, prefLen, "listen", 6) == 0) {
			const char* s = ct.data + prefLen + 1;
			int len1 = ct.datalen - prefLen - 1;
			const char* tmp = (const char*) memchr(s, ':', len1);
			if (tmp == NULL) throw ParserException_internal(ct, "expected \":\"");
			int x = tmp - s;
			for (int i = 0; i < (int) sd.listens.size(); i++) {
				//cout << "compared " << sd.listens[i].host << " " << sd.listens[i].port << endl;
				if (mystrcmp(sd.listens[i].host.data(), sd.listens[i].host.length(), s, x) == 0
						&& mystrcmp(sd.listens[i].port.data(), sd.listens[i].port.length(), s + x + 1,
								len1 - x - 1) == 0) {
					b.listenID = sd.listens[i].id;
					goto sssss;
				}
			}
			throw ParserException_internal(ct,
					"the specified listen directive \"" + string(s, len1) + "\" was not found");
			sssss: b.matchLevel |= binding::match_listenID;
		} else if (mystrcmp(ct.data, prefLen, "httppath", 8) == 0) {
			b.httpPath = string(ct.data + prefLen + 1, ct.datalen - prefLen - 1);
			b.matchLevel |= binding::match_httpPath;
		} else if (mystrcmp(ct.data, prefLen, "httphost", 8) == 0) {
			b.httpHost = string(ct.data + prefLen + 1, ct.datalen - prefLen - 1);
			b.matchLevel |= binding::match_httpHost;
		} else throw ParserException_internal(ct,
				"expected \"listen\", \"httppath\", or \"httphost\" directive, but got \""
						+ string(ct.data, prefLen) + "\"");
	}
	void loadConfig(const char* conf, int len, socketd& sd) {
		char state = 0;

		vhost* vh;
		binding* b;
		int maxListenID = 0;

		parseConfig(conf, len, [&](const configToken& ct) {
			//printf("configToken %i: %s\n",ct.type,string(ct.data,ct.datalen).c_str());
				switch(ct.type) {
					case configToken::t_beginBlock:
					{
						switch(state) {
							case 0:
							{
								int prefLen=configGetPrefix(ct.data,ct.datalen);
								if(mystrcmp(ct.data,prefLen,"vhost",5)==0) {
									state='v';
									sd.vhosts.resize(sd.vhosts.size()+1);
									vh=&*(sd.vhosts.end()-1);
									if (ct.datalen - prefLen - 1 > 0) {
										vh->name=string(ct.data + prefLen + 1,ct.datalen - prefLen - 1);
									}
								} else if(mystrcmp(ct.data,prefLen,"binding",7)==0) {
									if (ct.datalen - prefLen - 1 <= 0) throw ParserException_internal(ct, "missing name in \"binding\" block");
									sd.extraBindings.resize(sd.extraBindings.size()+1);
									b=&*(sd.extraBindings.end()-1);
									b->matchLevel=0;
									b->vhostName=string(ct.data + prefLen + 1,ct.datalen - prefLen - 1);
									state='d';
								} else throw ParserException_internal(ct,"expected \"vhost\" or \"binding\" block, but got \""+string(ct.data,prefLen)+"\"");
								break;
							}
							case 'v':
							{
								int prefLen=configGetPrefix(ct.data,ct.datalen);
								if(mystrcmp(ct.data,prefLen,"bindings",8)==0) {
									state='b';
								} else throw ParserException_internal(ct,"expected \"bindings\" block, but got \""+string(ct.data,prefLen)+"\"");
								break;
							}
							case 'b':
							{
								vh->bindings.resize(vh->bindings.size()+1);
								b=&*(vh->bindings.end()-1);
								b->matchLevel=0;
								state='c';
								break;
							}
							case 'c':
							{
								throw ParserException_internal(ct,"unexpected \"{\" when in \"binding\" section");
							}
						}
						break;
					}
					case configToken::t_endBlock:
					{
						switch(state) {
							case 0:
							throw ParserException_internal(ct,"unexpected \"}\"");
							case 'v':
							state=0; break;
							case 'b':
							state='v'; break;
							case 'c':
							state='b'; break;
							case 'd':
							state=0; break;
						}
						break;
					}
					case configToken::t_line:
					{
						switch(state) {
							case 0:
							{
								int prefLen=configGetPrefix(ct.data,ct.datalen);
								if(mystrcmp(ct.data,prefLen,"listen",6)==0) {
									sd.listens.resize(sd.listens.size()+1);
									listen* l;
									l=&*(sd.listens.end()-1);
									l->id=(++maxListenID);
									int ind=0;
									split(ct.data+prefLen,ct.datalen-prefLen,' ',[&](const char* s, int len) {
												if(len<=0)return;
												//cout << "listen directive token: " << string(s,len) << endl;
												switch(ind) {
													case 0: //address
													{
														const char* tmp=(const char*)memchr(s,':',len);
														if(tmp==NULL)throw ParserException_internal(ct,"expected \":\"");
														int i=tmp - s;
														l->host=string(s,i);
														l->port=string(s+i+1,len-i-1);
														//cout << "host: " << l->host << " port: " << l->port << endl;
														break;
													}
													case 1: //backlog
													{
														//make a copy because atoi() expects a null byte
														string tmp(s,len);
														l->backlog=atoi(tmp.c_str());
														break;
													}
													default:
													throw ParserException_internal(ct,"trailing garbage in \"listen\" directive");
													break;
												}
												ind++;
											});
								} else if(mystrcmp(ct.data,prefLen,"ipcbuffersize",13)==0) {
									if(ct.datalen-prefLen-1<=0) throw ParserException_internal(ct,"missing parameter in \"ipcbuffersize\" directive");
									sd.ipcBufSize=atoi(string(ct.data+prefLen+1,ct.datalen-prefLen-1).c_str());
								} else if(mystrcmp(ct.data,prefLen,"threads",7)==0) {
									if(ct.datalen-prefLen-1<=0) throw ParserException_internal(ct,"missing parameter in \"threads\" directive");
									sd.threads=atoi(string(ct.data+prefLen+1,ct.datalen-prefLen-1).c_str());
								} else throw ParserException_internal(ct,"expected \"listen\" or \"ipcbuffersize\" directive, but got \""+string(ct.data,prefLen)+"\"");
								break;
							}
							case 'v':
							{
								int prefLen=configGetPrefix(ct.data,ct.datalen);
								if(mystrcmp(ct.data,prefLen,"exec",4)==0) {
									if(ct.datalen-prefLen-1<=0) throw ParserException_internal(ct,"missing parameter in \"exec\" directive");
									vh->exepath=string(ct.data+prefLen+1,ct.datalen-prefLen-1);
								} else if(mystrcmp(ct.data,prefLen,"shell",5)==0) {
									if(ct.datalen-prefLen-1<=0) throw ParserException_internal(ct,"missing parameter in \"shell\" directive");
									vh->useShell=ct.data[prefLen+1]=='1';
								} else if(mystrcmp(ct.data,prefLen,"preload",7)==0) {
									if(ct.datalen-prefLen-1<=0) throw ParserException_internal(ct,"missing parameter in \"preload\" directive");
									vh->preload=ct.data[prefLen+1]=='1';
								} else if(mystrcmp(ct.data,prefLen,"authcookie",10)==0) {
									if(ct.datalen-prefLen-1<=0) throw ParserException_internal(ct,"missing parameter in \"authcookie\" directive");
									vh->authCookie=string(ct.data+prefLen+1,ct.datalen-prefLen-1);
								} else if(mystrcmp(ct.data,prefLen,"processes",9)==0) {
									if(ct.datalen-prefLen-1<=0) throw ParserException_internal(ct,"missing parameter in \"processes\" directive");
									vh->processes=atoi(string(ct.data+prefLen+1,ct.datalen-prefLen-1).c_str());
								} else if(mystrcmp(ct.data,prefLen,"ipcbuffersize",13)==0) {
									if(ct.datalen-prefLen-1<=0) throw ParserException_internal(ct,"missing parameter in \"ipcbuffersize\" directive");
									vh->ipcBufSize=atoi(string(ct.data+prefLen+1,ct.datalen-prefLen-1).c_str());
								} else throw ParserException_internal(ct,"expected \"exec\", \"shell\", \"preload\", \"authcookie\", \"processes\", or \"ipcbuffersize\" directive, but got \""+string(ct.data,prefLen)+"\"");
								break;
							}
							case 'b':
							{
								throw ParserException_internal(ct,"unexpected directive when in \"bindings\" section");
								break;
							}
							case 'c':
							{
								parseBindingDirective(*b,sd,ct);
								break;
							}
							case 'd':
							{
								parseBindingDirective(*b,sd,ct);
								break;
							}
						}
						break;
					}
				}
			});
		if (state != 0) throw ParserException_internal("got EOF while searching for matching \"}\"");
	}
	void reloadConfig(const char* conf, int len, socketd& sd) {

	}
}

