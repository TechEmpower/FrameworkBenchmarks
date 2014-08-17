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
 * common.C
 *
 *  Created on: Apr 6, 2013
 *      Author: xaxaxa
 */
#include <cpoll/cpoll.H>
#include <string>
#include <string.h>
#include <exception>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/types.h>
#include <stdexcept>
#include <fcntl.h>
#include <dlfcn.h>
#include <libgen.h>
#include <sys/wait.h>
#include "include/common.H"
#include "include/page.H"
#include <errno.h>
#include "include/split.H"

using namespace CP;
using namespace std;
namespace cppsp
{
	PThreadMutex dlMutex;
	const char* gxx = "g++";
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
	CompileException::CompileException() :
			message("Compilation error") {
	}
	CompileException::CompileException(string message) :
			message(message) {
	}
	CompileException::~CompileException() throw () {
	}
	const char* CompileException::what() const throw () {
		return message.c_str();
	}

	//inline-able memcpy() for copying SHORT STRINGS ONLY
	static inline void memcpy2(void* dst, const void* src, int len) {
		for (int i = 0; i < len; i++)
			((char*) dst)[i] = ((const char*) src)[i];
	}
	// "/"		+ "aaaaa"	=> "/aaaaa"
	// "/asdf/"	+ "zzz"		=> "/asdf/zzz"
	// "/asdf/"	+ "zzz/"		=> "/asdf/zzz/"
	// "/asdf"	+ "zzz"		=> "/zzz"
	// "/asdf/"	+ "../zzz"	=> "/zzz"
	// "/asdf/"	+ "a/../x"	=> "/asdf/x"
	// "/asdf/"	+ "/zzz"		=> "/zzz"
	//the size of buf should be at least strlen(p1)+strlen(p2)
	//returns the length of the string written to buf; does NOT write null byte
	int combinePath(const char* p1, int l1, const char* p2, int l2, char* buf) {
		if (l2 > 0 && p2[0] == '/') {
			memcpy2(buf, p2, l2);
			return l2;
		}
		int i = l1;
		memcpy2(buf, p1, i);
		if (l2 > 0) {
			i--;
			while (i >= 0 && buf[i] != '/')
				i--;
			if (i < 0) i = 0;
			split spl(p2, l2, '/');
			while (spl.read()) {
				const char* s = spl.value.data();
				int l = spl.value.length();
				if (l == 2 && *(const uint16_t*) s == *(const uint16_t*) "..") {
					i--;
					while (i >= 0 && buf[i] != '/')
						i--;
					if (i < 0) i = 0;
				} else if (l == 1 && *s == '.') {
					buf[i] = '/';
					i++;
				} else {
					//while(i>=0 && buf[i]!='/')i--;
					buf[i] = '/';
					i++;
					memcpy2(buf + i, s, l);
					i += l;
				}
			}
			//if (l2 > 0 && i > 0 && p2[l2 - 1] != '/' && buf[i - 1] == '/')
		}
		if (i < 0) i = 0;
		return i;
	}
	int combinePath(const char* p1, const char* p2, char* buf) {
		return combinePath(p1, strlen(p1), p2, strlen(p2), buf);
	}
	//p1 is the "root" directory
	//guarantees that the resulting path won't be outside of p1
	int combinePathChroot(const char* p1, int l1, const char* p2, int l2, char* buf) {
		int i = l1;
		memcpy2(buf, p1, i);
		static const uint16_t parentDir = *(const uint16_t*) "..";
		if (l2 > 0) {
			bool first(true);
			split spl(p2, l2, '/');
			while (spl.read()) {
				const char* s = spl.value.data();
				int l = spl.value.length();
				if (first) {
					first = false;
					if (l == 0) continue;
				}
				if (l == 2 && *(const uint16_t*) s == parentDir) {
					i--;
					while (i >= 0 && buf[i] != '/')
						i--;
					if (i < l1) i = l1;
				} else if (l == 1 && *s == '.') {
					if (!(i > 0 && buf[i - 1] == '/')) {
						buf[i] = '/';
						i++;
					}
				} else {
					//while(i>=0 && buf[i]!='/')i--;
					if (!(i > 0 && buf[i - 1] == '/')) {
						buf[i] = '/';
						i++;
					}
					memcpy2(buf + i, s, l);
					i += l;
				}
			}
			//if (l2 > 0 && i > 0 && p2[l2 - 1] != '/' && buf[i - 1] == '/')
		}
		if (i < l1) i = l1;
		return i;
	}
	int combinePathChroot(const char* p1, const char* p2, char* buf) {
		return combinePathChroot(p1, strlen(p1), p2, strlen(p2), buf);
	}

	String combinePath(String p1, String p2, StringPool& sp) {
		char* tmp = sp.beginAdd(p1.length() + p2.length());
		int l = combinePath(p1.data(), p2.data(), tmp);
		sp.endAdd(l);
		return {tmp,l};
	}
	String combinePathChroot(String p1, String p2, StringPool& sp) {
		char* tmp = sp.beginAdd(p1.length() + p2.length());
		int l = combinePathChroot(p1.data(), p2.data(), tmp);
		sp.endAdd(l);
		return {tmp,l};
	}
	//parses a cppsp page and generates code (to out) and string table (to st_out)
	void doParse(const char* name, const char* in, int inLen, Stream& out, Stream& st_out,
			vector<string>& c_opts) {
		const char* s = in;
		const char* end = s + inLen;
		string inherits = "Page";
		string classname = (name == NULL ? "__cppsp_unnamed_page" : name);
		int st_pos = 0;
		int st_len = 0;
		//int out_initlen=out.length();
		Stream& ms1(out); //declarations outside of the class
		MemoryStream ms2; //declarations outside of the render() function
		MemoryStream ms3; //code inside render()
		StreamWriter sw1(ms1);
		StreamWriter sw2(ms2);
		StreamWriter sw3(ms3);
		sw1.write("#include <cppsp/page.H>\n#include <cpoll/cpoll.H>\n");
		sw1.write("#include <cppsp/common.H>\n#include <cppsp/stringutils.H>\n");
		sw1.write("#include <rgc.H>\n");
		sw1.write("using namespace cppsp;\nusing namespace CP;\n");
		int line = 1;
		while (true) {
			if (s >= end) break;
			const char* old_s = s;
			s = (const char*) memmem(s, end - s, "<%", 2);

			if (s > old_s) {
				st_out.write(old_s, s - old_s);
				st_len += (s - old_s);
			} else if (s == NULL) {
				st_out.write(old_s, end - old_s);
				st_len += (end - old_s);

				sw3.writeF("__writeStringTable(%i,%i);\n", st_pos, st_len - st_pos);
				break;
			}
			for (const char* ch = old_s; ch < s; ch++)
				if (*ch == '\n') line++;
			s += 2;
			if (s >= end) throw ParserException("reached EOF when looking past \"<%\"");
			const char* s1 = (const char*) memmem(s, end - s, "%>", 2);
			if (s1 == NULL) throw ParserException("reached EOF when looking for matching \"%>\"");

			switch (*s) {
				case '!':
				{ //compiler option
					c_opts.push_back(string(s + 1, s1 - s - 1));
					break;
				}
				case '@':
				{ //cppsp options
					int nextopt = 0;
					split spl(s + 1, s1 - s - 1, ' ');
					while (spl.read()) {
						const char* s1 = spl.value.data();
						int l1 = spl.value.length();
						switch (nextopt) {
							case 0:
							{
								if (l1 == 8 && memcmp(s1, "inherits", 8) == 0) nextopt = 1;
								else if (l1 == 5 && memcmp(s1, "class", 5) == 0) nextopt = 2;
								continue;
							}
							case 1:
							{
								inherits = string(s1, l1);
								break;
							}
							case 2:
							{
								if (name == NULL) classname = string(s1, l1);
								break;
							}
						}
						nextopt = 0;
					}
					break;
				}
				case '#':
				{ //declarations outside of the class
					sw1.writeF("#line %i\n", line);
					sw1.write(s + 1, s1 - s - 1);
					break;
				}
				case '$':
				{ //declarations outside of the render() function
					sw2.writeF("#line %i\n", line);
					sw2.write(s + 1, s1 - s - 1);
					break;
				}
				case '=':
				{
					sw3.writeF("__writeStringTable(%i,%i);\n", st_pos, st_len - st_pos);
					st_pos = st_len;

					sw3.writeF("#line %i\n", line);
					sw3.write("output.write(");
					sw3.write(s + 1, s1 - s - 1);
					sw3.write(");\n");
					break;
				}
				default:
				{
					sw3.writeF("__writeStringTable(%i,%i);\n", st_pos, st_len - st_pos);
					st_pos = st_len;

					sw3.writeF("#line %i\n", line);
					sw3.write(s, s1 - s);
					break;
				}
			}
			for (const char* ch = s; ch < s1; ch++)
				if (*ch == '\n') line++;
			s = s1 + 2;
		}
		sw2.flush();
		sw3.flush();
		sw1.writeF("class %s: public %s {\npublic:\n", classname.c_str(), inherits.c_str());
		sw1.write(ms2.data(), ms2.length());
		//the name of the StreamWriter parameter should always be "output" -- this is part of
		//the cppsp API and should not ever be changed; users can rely on its name being "output".
		sw1.write("virtual void render(StreamWriter& output) override {\n");
		sw1.write(ms3.data(), ms3.length());
		sw1.write("}\n};\n");
		sw1.writeF("extern \"C\" int getObjectSize() {return sizeof(%s);}\n", classname.c_str());
		sw1.writeF("extern \"C\" Page* createObject(void* mem) {"
				"if(mem==NULL) return new %s(); else return new (mem) %s();}\n", classname.c_str(),
				classname.c_str());
		sw1.writeF("extern \"C\" Page* createObject1(RGC::Allocator* alloc) {"
				"%s* tmp = new (alloc->alloc(sizeof(%s))) %s(); tmp->allocator=alloc; return tmp;}\n",
				classname.c_str(), classname.c_str(), classname.c_str());
		sw1.flush();
		//out.write(ms1.data(), ms1.length());
	}
	static inline int checkError(int i) {
		if (i < 0) throw runtime_error(strerror(errno));
		return i;
	}
	static inline void* checkError(void* p) {
		if (p == NULL) throw runtime_error(strerror(errno));
		return p;
	}
	static inline void* checkDLError(void* p) {
		if (p == NULL) throw runtime_error(dlerror());
		return p;
	}
	CP::File* compilePage(string wd, string path, string cPath, string txtPath, string output,
			const vector<string>& cxxopts, pid_t& pid, string& compilecmd) {
		vector<string> c_opts { gxx, gxx, "--std=c++0x", "--shared", "-o", output, cPath };
		c_opts.insert(c_opts.end(), cxxopts.begin(), cxxopts.end());
		{
			File inp(open(path.c_str(), O_RDONLY));
			MemoryStream ms;
			inp.readToEnd(ms);
			ms.flush();
			//unlink((path + ".C").c_str());
			File out_c(open(cPath.c_str(), O_RDWR | O_CREAT | O_TRUNC, 0666));
			File out_s(open(txtPath.c_str(), O_RDWR | O_CREAT | O_TRUNC, 0666));
			cppsp::doParse(NULL, (const char*) ms.data(), ms.length(), out_c, out_s, c_opts);
		}

		const char* cmds[c_opts.size() + 1];
		for (int i = 0; i < (int) c_opts.size(); i++) {
			cmds[i] = c_opts[i].c_str();
		}
		cmds[c_opts.size()] = NULL;

		int p[2];
		checkError(pipe(p));
		//unlink((path + ".so").c_str());
		pid_t tmp = fork();
		if (tmp > 0) {
			close(p[1]);
			pid = tmp;
			for (int i = 1; i < (int) c_opts.size(); i++) {
				compilecmd.append(c_opts[i]);
				compilecmd.append(" ");
			}
			return new CP::File(p[0]);
		} else if (tmp == 0) {
			close(p[0]);
			dup2(p[1], 1);
			dup2(p[1], 2);
			close(p[1]);
			chdir(wd.c_str());
			execvp(cmds[0], (char**) (cmds + 1));
			_exit(1);
		} else {
			checkError(tmp);
		}
		return NULL;
	}
	int tsCompare(struct timespec time1, struct timespec time2) {
		if (time1.tv_sec < time2.tv_sec) return (-1); /* Less than. */
		else if (time1.tv_sec > time2.tv_sec) return (1); /* Greater than. */
		else if (time1.tv_nsec < time2.tv_nsec) return (-1); /* Less than. */
		else if (time1.tv_nsec > time2.tv_nsec) return (1); /* Greater than. */
		else return (0); /* Equal. */
	}

#define CONCAT_TO(in,inLen,out,conc) char out[strlen(conc)+inLen+1];\
	memcpy(out,in,inLen);\
	memcpy(out+inLen,conc,strlen(conc));\
	out[strlen(conc)+inLen]=0;
#define TO_C_STR(in,inLen,out) char out[inLen+1];\
	memcpy(out,in,inLen);\
	out[inLen]=0;
	static inline void pageErr_isDir() {
		throw ParserException("requested path is a directory or socket");
	}
	void loadedPage::readCB(int r) {
		if (r <= 0) {
			compiling = false;
			int status = -1;
			waitpid(compilerPID, &status, 0);

			//keep a copy of the compiled page
			if (status == 0) {
				unlink(cPath.c_str());
				string dll1 = dllPath + ".1";
				link(dllPath.c_str(), dll1.c_str());
				rename(dll1.c_str(), (path + ".so").c_str());
				string txt1 = txtPath + ".1";
				link(txtPath.c_str(), txt1.c_str());
				rename(txt1.c_str(), (path + ".txt").c_str());
			}
			afterCompile(status == 0);
			compile_fd = nullptr;
			//if (compileCB != nullptr) compileCB(*this);
			return;
		}
		ms.bufferPos += r;
		ms.flush();
		beginRead();
	}
	void loadedPage::deleteTmpfiles() {
		unlink(txtPath.c_str());
		unlink(dllPath.c_str());
	}
	void loadedPage::afterCompile(bool success) {
		if (!success) {
			rename(cPath.c_str(), (path + ".C").c_str());
			deleteTmpfiles();
			CompileException exc;
			exc.compilerOutput = string((const char*) ms.data(), ms.length());
			auto tmpcb = loadCB;
			loadCB.clear();
			for (int i = 0; i < (int) tmpcb.size(); i++)
				tmpcb[i](nullptr, &exc);
			return;
		}
		try {
			if (loaded) doUnload();
			doLoad();
		} catch (exception& ex) {
			deleteTmpfiles();
			auto tmpcb = loadCB;
			loadCB.clear();
			for (int i = 0; i < (int) tmpcb.size(); i++)
				tmpcb[i](nullptr, &ex);
			return;
		}
		deleteTmpfiles();
		auto tmpcb = loadCB;
		loadCB.clear();
		for (int i = 0; i < (int) tmpcb.size(); i++)
			tmpcb[i](this, nullptr);
	}
	void loadedPage::beginRead() {
		if (ms.bufferSize - ms.bufferPos < 4096) ms.flushBuffer(4096);
		compile_fd->read(ms.buffer + ms.bufferPos, ms.bufferSize - ms.bufferPos,
				CP::Callback(&loadedPage::readCB, this));
	}
	void loadedPage::doCompile(Poll& p, string wd, const vector<string>& cxxopts) {
		string tmp;
		char sss[32];
		snprintf(sss, 32, "%i", rand());
		txtPath = path + "." + string(sss) + ".txt";
		dllPath = path + "." + string(sss) + ".dll";
		cPath = path + "." + string(sss) + ".C";

		//check if a precompiled page exists; if it exists and is newer than
		//the .cppsp, then simply hardlink to it
		timespec modif_txt, modif_so, modif_cppsp;
		struct stat st;
		{
			TO_C_STR(path.data(), path.length(), s1);
			if (stat(s1, &st) < 0) goto do_comp;
			modif_cppsp = st.st_mtim;
		}
		{
			CONCAT_TO(path.data(), path.length(), s1, ".txt");
			if (stat(s1, &st) < 0) goto do_comp;
			modif_txt = st.st_mtim;
		}
		{
			CONCAT_TO(path.data(), path.length(), s1, ".so");
			if (stat(s1, &st) < 0) goto do_comp;
			modif_so = st.st_mtim;
		}
		if (tsCompare(modif_cppsp, modif_txt) <= 0 && tsCompare(modif_cppsp, modif_so) <= 0) {
			string txt1 = path + ".txt";
			string dll1 = path + ".so";
			if (link(txt1.c_str(), txtPath.c_str()) < 0) goto do_comp;
			if (link(dll1.c_str(), dllPath.c_str()) < 0) goto do_comp;
			afterCompile(true);
			return;
		}

		do_comp: deleteTmpfiles();
		CP::File* f;
		ms.clear();
		try {
			f = (CP::File*) checkError(
					compilePage(wd, path, cPath, txtPath, dllPath, cxxopts, compilerPID, tmp));
		} catch (...) {
			deleteTmpfiles();
			throw;
		}
		tmp += "\n";
		ms.write(tmp.data(), tmp.length());
		p.add(*f);
		compile_fd = f;
		f->release();
		beginRead();
		compiling = true;
	}
	void loadedPage::doLoad() {
		ScopeLock sl(dlMutex);
		//printf("doLoad(\"%s\");\n",path.c_str());
		struct stat st;
		checkError(stat(txtPath.c_str(), &st));
		stringTableLen = st.st_size;
		stringTableFD = checkError(open(txtPath.c_str(), O_RDONLY));
		stringTable = (const uint8_t*) checkError(
				mmap(NULL, stringTableLen, PROT_READ, MAP_SHARED, stringTableFD, 0));
		dlHandle = dlopen(dllPath.c_str(), RTLD_LOCAL | RTLD_LAZY);
		if (dlHandle == NULL) throw runtime_error(dlerror());
		getObjectSize = (getObjectSize_t) checkDLError(dlsym(dlHandle, "getObjectSize"));
		createObject = (createObject_t) checkDLError(dlsym(dlHandle, "createObject"));
		createObject1 = (createObject1_t) checkDLError(dlsym(dlHandle, "createObject1"));
		initModule_t initModule = (initModule_t) dlsym(dlHandle, "initModule");
		if (initModule != NULL) {
			initModule(srv);
			fprintf(stderr, "module %s loaded\n", path.c_str());
		}
		loaded = true;
		clock_gettime(CLOCK_REALTIME, &lastLoad);
		//printf("loaded: dlHandle=%p; createObject=%p\n",dlHandle,(void*)createObject);
	}
	void loadedPage::doUnload() {
		//printf("doUnload(\"%s\");\n",path.c_str());
		loaded = false;
		if (stringTable != NULL) munmap((void*) stringTable, stringTableLen);
		if (stringTableFD != -1) close(stringTableFD);
		if (dlHandle != NULL) {
			deinitModule_t deinitModule = (deinitModule_t) dlsym(dlHandle, "deinitModule");
			if (deinitModule != NULL) {
				deinitModule();
				fprintf(stderr, "module %s unloaded\n", path.c_str());
			}
			ScopeLock sl(dlMutex);
			checkError(dlclose(dlHandle));
			//void* tmp=dlopen((path + ".so").c_str(), RTLD_LOCAL | RTLD_LAZY|RTLD_NOLOAD);
			//if(tmp!=NULL) throw runtime_error("unable to unload library");
		}
		dlHandle = NULL;
		stringTable = NULL;
		stringTableFD = -1;
		//printf("unloaded\n");
	}
	Page* loadedPage::doCreate(RGC::Allocator* a) {
		Page* tmp = createObject1(a);
		checkError(tmp);
		tmp->__stringTable = stringTable;
		tmp->filePath = {path.data(),(int)path.length()};
		return tmp;
	}
	loadedPage::loadedPage() :
			dlHandle(NULL), stringTable(NULL), stringTableFD(-1) {
		//printf("loadedPage()\n");
		compiling = false;
		doUnload();
	}
	loadedPage::~loadedPage() {
		//printf("~loadedPage()\n");
		doUnload();
	}
	//returns: 0: no-op; 1: should reload; 2: should recompile
	int loadedPage::shouldCompile() {
		struct stat st;
		{
			TO_C_STR(path.data(), path.length(), s1);
			checkError(stat(s1, &st));
			if (S_ISDIR(st.st_mode) || S_ISSOCK(st.st_mode)) pageErr_isDir();
		}
		if (!loaded) return 2;
		timespec modif_cppsp = st.st_mtim;
		/*{
		 CONCAT_TO(path.data(), path.length(), s1, ".txt");
		 if (stat(s1, &st) < 0) {
		 if (errno == ENOENT) return 2;
		 else checkError(-1);
		 }
		 }
		 timespec modif_txt = st.st_mtim;
		 {
		 CONCAT_TO(path.data(), path.length(), s1, ".so");
		 if (stat(s1, &st) < 0) {
		 if (errno == ENOENT) return 2;
		 else checkError(-1);
		 }
		 }
		 timespec modif_so = st.st_mtim;*/
		int i = 0;
		if (tsCompare(lastLoad, modif_cppsp) < 0) i = 2;
		//if(tsCompare(lastLoad, modif_txt)< 0 || tsCompare(lastLoad, modif_so) <0) i=1;
		//if(tsCompare(modif_cppsp, modif_txt)> 0 || tsCompare(modif_cppsp, modif_so) >0) i=2;
		//printf("shouldCompile(\"%s\") = %i\n",path.c_str(),i);
		return i;
	}
	void staticPage::doLoad() {
		struct stat st;
		checkError(stat(path.c_str(), &st));
		data.len = st.st_size;
		int fd = checkError(open(path.c_str(), O_RDONLY));
		data.d = (char*) checkError(mmap(NULL, data.len, PROT_READ, MAP_SHARED, fd, 0));
		close(fd);
		loaded = true;
		clock_gettime(CLOCK_REALTIME, &lastLoad);
	}
	void staticPage::doUnload() {
		loaded = false;
		if (data.d != NULL) munmap((void*) data.d, data.len);
		data = nullptr;
	}
	bool staticPage::shouldReload() {
		struct stat st;
		{
			TO_C_STR(path.data(), path.length(), s1);
			checkError(stat(s1, &st));
			if (S_ISDIR(st.st_mode) || S_ISSOCK(st.st_mode)) throw ParserException(
					"requested path is a directory or socket");
		}
		if (!loaded) return true;
		timespec modif_cppsp = st.st_mtim;
		return (tsCompare(lastLoad, modif_cppsp) < 0);
	}
	staticPage::staticPage() {
		loaded = false;
	}
	staticPage::~staticPage() {
		doUnload();
	}

	inline void loadedPage::_loadCB::operator()(loadedPage* This, exception* ex) {
		if (ex == NULL) {
			void* p;
			try {
				p = doCreate ? This->doCreate(alloc) : This->dlHandle;
			} catch (exception& x) {
				cb(nullptr, &x);
				return;
			}
			cb(p, nullptr);
		} else cb(nullptr, ex);
	}

	static inline void precheckPage(String path) {
		struct stat st;
		TO_C_STR(path.data(), path.length(), s1);
		checkError(stat(s1, &st));
		if (S_ISDIR(st.st_mode) || S_ISSOCK(st.st_mode)) pageErr_isDir();
	}
	cppspManager::cppspManager() :
			threadID(0) {
		curRFCTime.d = (char*) malloc(32);
		curRFCTime.len = 0;
	}
	cppspManager::~cppspManager() {
		free(curRFCTime.d);
	}
	String cppspManager::loadStaticPage(String path) {
		staticPage* lp1;
		auto it = staticCache.find(path);
		if (it == staticCache.end()) {
			precheckPage(path);
			lp1 = new staticPage();
			lp1->path = path.toSTDString();
			staticCache.insert( { sp.addString(path), lp1 });
		} else lp1 = (*it).second;
		staticPage& lp(*lp1);
		if (likely(lp.loaded & !shouldCheck(lp))) {
			return lp.data;
		}
		if (lp.shouldReload()) {
			lp.doUnload();
		}
		if (!lp.loaded) lp.doLoad();
		return lp.data;
	}
	bool cppspManager::shouldCheck(loadedPage& p) {
		timespec tmp1 = curTime;
		tmp1.tv_sec -= 2;
		if (tsCompare(p.lastCheck, tmp1) < 0) {
			p.lastCheck = curTime;
			return true;
		} else return false;
	}
	bool cppspManager::shouldCheck(staticPage& p) {
		timespec tmp1 = curTime;
		tmp1.tv_sec -= 2;
		if (tsCompare(p.lastCheck, tmp1) < 0) {
			p.lastCheck = curTime;
			return true;
		} else return false;
	}

	void cppspManager::loadPage(Poll& p, String wd, String path, RGC::Allocator* a,
			Delegate<void(Page*, exception* ex)> cb) {
		loadedPage* lp1;
		auto it = cache.find(path);
		if (unlikely(int(it == cache.end()))) {
			//check if file exists; if not, don't construct loadedPage object to avoid DoS
			try {
				precheckPage(path);
			} catch (exception& ex) {
				cb(nullptr, &ex);
				return;
			}
			lp1 = new loadedPage();
			lp1->path = path.toSTDString();
			cache.insert( { sp.addString(path), lp1 });
		} else lp1 = (*it).second;
		loadedPage& lp(*lp1);

		int c = 0;
		if (unlikely(lp1->compiling)) {
			lp.loadCB.push_back( { a, Delegate<void(void*, exception* ex)>(cb), true });
			return;
		}
		if (likely(lp1->loaded & !shouldCheck(*lp1))) {
			xxx: Page* p;
			try {
				p = lp.doCreate(a);
			} catch (exception& ex) {
				cb(nullptr, &ex);
			}
			cb(p, nullptr);
			return;
		}
		try {
			c = lp.shouldCompile();
			if (likely(lp1->loaded && c==0)) goto xxx;
		} catch (exception& ex) {
			cb(nullptr, &ex);
			return;
		}
		if (c >= 2) {
			lp.loadCB.push_back( { a, Delegate<void(void*, exception* ex)>(cb), true });
			try {
				lp.doCompile(p, wd.toSTDString(), cxxopts);
			} catch (exception& ex) {
				lp.loadCB.resize(lp.loadCB.size() - 1);
				cb(nullptr, &ex);
			}
			return;
		}
		try {
			if (c >= 1) {
				lp.doUnload();
			}
			lp.doLoad();
			goto xxx;
		} catch (exception& ex) {
			cb(nullptr, &ex);
		}
	}
	void cppspManager::loadModule(Poll& p, Server* srv, String wd, String path,
			Delegate<void(void*, exception* ex)> cb) {
		loadedPage* lp1;
		auto it = cache.find(path);
		if (unlikely(int(it == cache.end()))) {
			try {
				precheckPage(path);
			} catch (exception& ex) {
				cb(nullptr, &ex);
				return;
			}
			lp1 = new loadedPage();
			lp1->srv = srv;
			lp1->path = path.toSTDString();
			cache.insert( { sp.addString(path), lp1 });
		} else lp1 = (*it).second;
		loadedPage& lp(*lp1);

		int c = 0;
		if (unlikely(lp1->compiling)) {
			lp.loadCB.push_back( { nullptr, cb, false });
			return;
		}
		if (likely(lp1->loaded & !shouldCheck(*lp1))) {
			xxx: cb(lp1->dlHandle, nullptr);
			return;
		}
		try {
			c = lp.shouldCompile();
			if (likely(lp1->loaded && c==0)) goto xxx;
		} catch (exception& ex) {
			cb(nullptr, &ex);
			return;
		}
		if (c >= 2) {
			lp.loadCB.push_back( { nullptr, cb, false });
			try {
				lp.doCompile(p, wd.toSTDString(), cxxopts);
			} catch (exception& ex) {
				lp.loadCB.resize(lp.loadCB.size() - 1);
				cb(nullptr, &ex);
			}
			return;
		}
		try {
			if (c >= 1) {
				lp.doUnload();
			}
			lp.doLoad();
			goto xxx;
		} catch (exception& ex) {
			cb(nullptr, &ex);
		}
	}
	String loadStaticPage(cppspManager* mgr, String path) {
		return mgr->loadStaticPage(path);
	}
	vector<string>& CXXOpts(cppspManager* mgr) {
		return mgr->cxxopts;
	}
	cppspManager* cppspManager_new() {
		return new cppspManager();
	}
	void setThreadID(cppspManager* mgr, int tid) {
		mgr->threadID = tid;
	}
	void loadPage(cppspManager* mgr, CP::Poll& p, String wd, String path, RGC::Allocator* a,
			Delegate<void(Page*, exception* ex)> cb) {
		return mgr->loadPage(p, wd, path, a, cb);
	}
	void cppspManager_delete(cppspManager* mgr) {
		delete mgr;
	}
	void updateTime(cppspManager* mgr) {
		clock_gettime(CLOCK_MONOTONIC, &mgr->curTime);
		clock_gettime(CLOCK_REALTIME, &mgr->curClockTime);
		tm time;
		gmtime_r(&mgr->curClockTime.tv_sec, &time);
		mgr->curRFCTime.len = rfctime(time, mgr->curRFCTime.d);
	}
	
	void loadModule(cppspManager* mgr, CP::Poll& p, Server* srv, String wd, String path,
			Delegate<void(void*, exception* ex)> cb) {
		mgr->loadModule(p, srv, wd, path, cb);
	}
	
	void handleError(exception* ex, cppsp::Response& resp, String path) {
		resp.clear();
		resp.statusCode = 500;
		resp.statusName = "Internal server error";
		resp.headers["Content-Type"] = "text/html; charset=UTF-8";
		//resp.writeHeaders();
		string title = "Server error in " + path.toSTDString();
		resp.output.writeF("<html><head><title>%s</title>"
				"<style></style></head>", title.c_str());
		resp.output.writeF("<body><h1 style=\"color: #aa1111\">%s</h1><hr />"
				"<h2 style=\"color: #444\">%s</h2>", title.c_str(), ex->what());
		cppsp::CompileException* ce = dynamic_cast<cppsp::CompileException*>(ex);
		if (ce != NULL) {
			resp.output.write("<pre style=\"color: #000; background: #ffc; padding: 8px;\">");
			htmlEscape(String(ce->compilerOutput), resp.output);
			resp.output.write("</pre>");
		}
		resp.output.writeF("</body></html>");
	}
}
