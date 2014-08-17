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
 * common.H
 *
 *  Created on: Apr 6, 2013
 *      Author: xaxaxa
 */

#ifndef CPPSP_COMMON_H_
#define CPPSP_COMMON_H_
#include <cpoll/cpoll.H>
#include <vector>
#include <unordered_map>
#include "page.H"
using namespace std;
typedef CP::String String;
namespace cppsp
{
	extern const char* gxx;
	class ParserException: public std::exception
	{
	public:
		string message;
		int32_t number;
		ParserException();
		ParserException(int32_t number);
		ParserException(string message, int32_t number = 0);
		~ParserException() throw ();
		const char* what() const throw ();
	};
	class CompileException: public std::exception
	{
	public:
		string message;
		string compilerOutput;
		CompileException();
		CompileException(string message);
		~CompileException() throw ();
		const char* what() const throw ();
	};
	/**
	 Internal API.
	 */
	class loadedPage
	{
	public:
		Server* srv;
		loadedPage& operator=(const loadedPage& other) = delete;
		timespec lastLoad { 0, 0 }; //CLOCK_REALTIME
		timespec lastCheck { 0, 0 }; //CLOCK_MONOTONIC
		void* dlHandle;
		const uint8_t* stringTable;
		int stringTableLen;
		typedef int (*getObjectSize_t)();
		typedef Page* (*createObject_t)(void* mem);
		typedef Page* (*createObject1_t)(RGC::Allocator* alloc);
		typedef void (*initModule_t)(Server* s);
		typedef void (*deinitModule_t)();
		int (*getObjectSize)();
		createObject_t createObject;
		createObject1_t createObject1;
		RGC::Ref<CP::File> compile_fd;
		CP::MemoryStream ms;
		//Delegate<void(loadedPage&)> compileCB;
		struct _loadCB
		{
			RGC::Allocator* alloc;
			//void* parameter is either the constructed page (if doCreate is true), or
			//the dlopen()ed handle
			Delegate<void(void*, exception* ex)> cb;
			bool doCreate;
			void operator()(loadedPage* This, exception* ex);
		};
		vector<_loadCB> loadCB;
		string path;
		string cPath;
		string txtPath;
		string dllPath;
		int stringTableFD;
		pid_t compilerPID;
		bool loaded;
		bool compiling;
		void readCB(int r);
		void deleteTmpfiles();
		void afterCompile(bool success);
		void beginRead();
		void doCompile(CP::Poll& p, string wd, const vector<string>& cxxopts);
		void doLoad();
		void doUnload();
		Page* doCreate(RGC::Allocator* a);
		loadedPage();
		~loadedPage();
		//returns: 0: no-op; 1: should reload; 2: should recompile
		int shouldCompile();
	};
	/**
	 Internal API.
	 */
	struct staticPage
	{
		String data;
		timespec lastLoad { 0, 0 }; //CLOCK_REALTIME
		timespec lastCheck { 0, 0 }; //CLOCK_MONOTONIC
		timespec lastUse; //CLOCK_MONOTONIC
		string path;
		bool loaded;
		void doLoad();
		void doUnload();
		bool shouldReload();
		staticPage();
		~staticPage();
	};
	/**
	 Internal API.
	 */
	class cppspManager
	{
	public:
		CP::StringPool sp;
		unordered_map<String, loadedPage*> cache;

		unordered_map<String, staticPage*> staticCache;
		vector<string> cxxopts;
		timespec curTime { 0, 0 };
		timespec curClockTime { 0, 0 }; //CLOCK_REALTIME
		String curRFCTime;
		int threadID;
		cppspManager();
		~cppspManager();
		void loadPage(CP::Poll& p, String wd, String path, RGC::Allocator* a,
				Delegate<void(Page*, exception* ex)> cb);
		void loadModule(CP::Poll& p, Server* srv, String wd, String path,
				Delegate<void(void*, exception* ex)> cb);
		String loadStaticPage(String path);
		bool shouldCheck(loadedPage& p);
		bool shouldCheck(staticPage& p);
	};

	/**
	 Internal function. Parses a .cppsp page.
	 @unmaintained_api
	 */
	void doParse(const char* name, const char* in, int inLen, CP::Stream& out, CP::Stream& st_out,
			vector<string>& c_opts);
	/**
	 Internal function. Compiles a .cppsp page.
	 @return file descriptor connected to the standard output of the compiler.
	 @unmaintained_api
	 */
	CP::File* compilePage(string wd, string path, const vector<string>& cxxopts, pid_t& pid);
	/**
	 Combine two paths. Result is stored in buf. Does not write a null byte.
	 The memory block pointed to by buf must be at least l1 + l2 bytes.
	 @return the length of the string written to buf
	 */
	int combinePath(const char* p1, int l1, const char* p2, int l2, char* buf);
	/**
	 Combine two paths. Result is stored in buf. Does not write a null byte.
	 The memory block pointed to by buf must be at least strlen(p1) + strlen(p2) bytes.
	 @return the length of the string written to buf
	 */
	int combinePath(const char* p1, const char* p2, char* buf);
	/**
	 Combine two paths securely. The resulting path is guaranteed to be under p1. (The user can not use ".." to leave the root directory)
	 The memory block pointed to by buf must be at least l1 + l2 bytes.
	 @return the length of the string written to buf
	 */
	int combinePathChroot(const char* p1, int l1, const char* p2, int l2, char* buf);
	/**
	 Combine two paths securely. The resulting path is guaranteed to be under p1. (The user can not use ".." to leave the root directory)
	 The memory block pointed to by buf must be at least strlen(p1) + strlen(p2) bytes.
	 @return the length of the string written to buf
	 */
	int combinePathChroot(const char* p1, const char* p2, char* buf);
	/**
	 Combine two paths. Result is allocated from sp.
	 @return the resulting path
	 */
	String combinePath(String p1, String p2, CP::StringPool& sp);
	/**
	 Combine two paths securely. The resulting path is guaranteed to be under p1. (The user can not use ".." to leave the root directory)
	 @return the resulting path
	 */
	String combinePathChroot(String p1, String p2, CP::StringPool& sp);
	/**
	 Internal function.
	 @unmaintained_api
	 */
	cppspManager* cppspManager_new();
	/**
	 Internal function.
	 @unmaintained_api
	 */
	void cppspManager_delete(cppspManager* mgr);
	/**
	 Internal function.
	 @unmaintained_api
	 */
	void loadPage(cppspManager* mgr, CP::Poll& p, String wd, String path, RGC::Allocator* a,
			Delegate<void(Page*, exception* ex)> cb);
	/**
	 Internal function.
	 @unmaintained_api
	 */
	void loadModule(cppspManager* mgr, CP::Poll& p, Server* srv, String wd, String path,
			Delegate<void(void*, exception* ex)> cb);
	/**
	 Internal function.
	 @unmaintained_api
	 */
	String loadStaticPage(cppspManager* mgr, String path);
	/**
	 Internal function.
	 @unmaintained_api
	 */
	vector<string>& CXXOpts(cppspManager* mgr);
	/**
	 Internal function.
	 @unmaintained_api
	 */
	void updateTime(cppspManager* mgr);
	/**
	 Internal function.
	 @unmaintained_api
	 */
	void handleError(exception* ex, cppsp::Response& resp, String path);
}
#endif /* COMMON_H_ */
