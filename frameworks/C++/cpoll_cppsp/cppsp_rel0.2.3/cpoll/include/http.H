/*
 * http.H
 *
 *  Created on: Apr 17, 2013
 *      Author: xaxaxa
 */

#ifndef HTTP_H_
#define HTTP_H_

#include <curl/curl.h>
#include <cpoll/cpoll.H>
#include <functional>
#include <queue>
#include <string>

using std::function;
using std::queue;
using std::string;

namespace curl
{
	struct transferInfo;
	struct instance
	{
		CP::Poll* p;
		CURLM *m;
		CP::Timer timer;
	};
	void newInstance(instance* inst, CP::Poll* p);
	void dispose(instance* inst);
	void checkQueue(instance* inst);
	void cb_event(int fd, short events, void *userdata);
	void addCurlTask(instance* inst, CURL* c, const function<void(CURL*, CURLcode)>& cb);
	transferInfo* addTransfer(instance* inst, const char* url,
			const function<bool(const void* data, int len, int state)>& cb);
	/*-1:failed 1:connected 2:sent 3:recving 4:closed*/
	void beginTransfer(instance* inst, transferInfo* t);
	class transferManager
	{
	public:
		instance inst;
		int itemsProcessing, concurrency;
		struct item
		{
			string url;
			bool post;
			function<bool(const void* data, int len, int state)> cb;
		};
		queue<item> q;
		transferManager(CP::Poll& p, int concurrency = 3) :
				itemsProcessing(0), concurrency(concurrency) {
			curl::newInstance(&inst, &p);
		}
		~transferManager() {
			curl::dispose(&inst);
		}
		void checkQueue();
		void doTransfer(const char* url, bool post,
				const function<bool(const void* data, int len, int state)>& cb);
		void addTransfer(const char* url, bool post,
				const function<bool(const void* data, int len, int state)>& cb);
	};

}

#endif /* HTTP_H_ */
