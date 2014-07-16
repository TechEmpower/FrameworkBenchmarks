/***************************************************************************
 *
 * copyright notice: some code is copied from cURL, some code is my own
 *                                  _   _ ____  _
 *  Project                     ___| | | |  _ \| |
 *                             / __| | | | |_) | |
 *                            | (__| |_| |  _ <| |___
 *                             \___|\___/|_| \_\_____|
 *
 * Copyright (C) 1998 - 2011, Daniel Stenberg, <daniel@haxx.se>, et al.
 *
 * This software is licensed as described in the file COPYING, which
 * you should have received as part of this distribution. The terms
 * are also available at http://curl.haxx.se/docs/copyright.html.
 *
 * You may opt to use, copy, modify, merge, publish, distribute and/or sell
 * copies of the Software, and permit persons to whom the Software is
 * furnished to do so, under the terms of the COPYING file.
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied.
 *
 * //modified by: xaxaxa
 *
 ***************************************************************************/

#include "include/http.H"
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <iostream>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include <sys/poll.h>
using namespace std;
using namespace CP;
namespace curl
{
	struct taskInfo;
	void processEvent(taskInfo* t, int i);
	struct taskInfo: public CP::File
	{
		instance* inst;
		CURL* c;
		int act; //from libcurl
		bool isAdded;
		taskInfo(HANDLE h) :
				File(h), act(0), isAdded(false) {
		}
		void inCB(int i) {
			processEvent(this, CURL_CSELECT_IN);
		}
		void outCB(int i) {
			processEvent(this, CURL_CSELECT_OUT);
		}
		void updateEvents(int act) {
			if (act & CURL_POLL_IN) waitForEvent(CP::Events::in, { &taskInfo::inCB, this }, true);
			else if (this->act & CURL_POLL_IN) cancelRead();
			if (act & CURL_POLL_OUT) waitForEvent(CP::Events::out, { &taskInfo::outCB, this }, true);
			else if (this->act & CURL_POLL_OUT) cancelWrite();
			this->act = act;
		}
		void setTaskInfo(instance* inst, CURL* c, int act) {
			this->inst = inst;
			this->c = c;

			updateEvents(act);
			inst->p->add(*this);
			this->isAdded = true;
		}
	};
	struct curlTaskInfo
	{
		CURL *c;
		function<void(CURL*, CURLcode)> cb;
		//void* userdata;
	};
	void checkQueue(instance* inst) {
		CURLMsg *msg;
		int msgs_left;
		while ((msg = curl_multi_info_read(inst->m, &msgs_left))) {
			if (msg->msg == CURLMSG_DONE) {
				CURL *c = msg->easy_handle;
				curlTaskInfo* t = NULL;
				curl_easy_getinfo(c, CURLINFO_PRIVATE, &t);
				//cout << "t = " << t << endl;
				curl_multi_remove_handle(inst->m, c);
				curl_easy_cleanup(c);
				t->cb(c, msg->data.result);
				delete t;
			}
		}
	}
	void processEvent(taskInfo* t, int i) {
		//printf("processEvent: i=%i\n", i);
		int num_transfers;
		curl_multi_socket_action(t->inst->m, t->handle, i, &num_transfers);
		checkQueue(t->inst);
	}

	/* CURLMOPT_SOCKETFUNCTION */
	int cb_sock(CURL *c, curl_socket_t s, int what, void *cbp, void *sockp) {
		//cout << "cb_sock()" << endl;

		instance* inst = (instance*) cbp;
		taskInfo* t = (taskInfo*) sockp;
		if (what == CURL_POLL_REMOVE && t != NULL) {
			//printf("cb_sock: remove\n");
			//if (t->isAdded) event_del(&t->ev);
			inst->p->del(*t);
			t->handle = -1;
			delete t;
		} else {
			if (t == NULL) { //add
				//printf("cb_sock: add\n");
				t = new taskInfo((HANDLE) s);
				t->setTaskInfo(inst, c, what);
				curl_multi_assign(inst->m, s, t);
			} else { //modify events monitored
				//printf("cb_sock: update\n");
				t->updateEvents(what);
			}
		}
		return 0;
	}
	void addCurlTask(instance* inst, CURL* c, const function<void(CURL*, CURLcode)>& cb) {
		//printf("addCurlTask\n");
		curlTaskInfo* t = new curlTaskInfo();
		t->c = c;
		t->cb = cb;
		//t->userdata=userdata;
		curl_easy_setopt(c, CURLOPT_PRIVATE, t);
		curl_multi_add_handle(inst->m, c);
		//int still_running;
		//curl_multi_perform(inst->m, &still_running);
	}
	struct transferInfo
	{
		function<bool(const void* data, int len, int state)> cb;
		CURL* c;
	};
	size_t cb_data(void *data, size_t size, size_t nmemb, void *userdata) {
		transferInfo* t = (transferInfo*) userdata;
		if (!t->cb(data, size * nmemb, 3)) return 0;
		return size * nmemb;
	}
	transferInfo* addTransfer(instance* inst, const char* url,
			const function<bool(const void* data, int len, int state)>& cb)
			/*-1:failed 1:connected 2:sent 3:recving 4:closed*/
			{
		CURL* c = curl_easy_init();
		curl_easy_setopt(c, CURLOPT_URL, url);
		transferInfo* t = new transferInfo();
		t->cb = cb;
		t->c = c;
		curl_easy_setopt(c, CURLOPT_WRITEDATA, t);
		curl_easy_setopt(c, CURLOPT_WRITEFUNCTION, cb_data);
		return t;
	}
	void beginTransfer(instance* inst, transferInfo* t) {
		addCurlTask(inst, t->c, [t](CURL* c,CURLcode res)
		{
			if(res==CURLE_OK) t->cb(NULL,0,4);
			else if(res!=CURLE_WRITE_ERROR) t->cb(NULL,0,-res);
			delete t;
		});
	}
	int cb_curl_timer(CURLM *m, long timeout_ms, void* userdata) { /* Update the event timer after curl_multi library calls */
		//printf("cb_curl_timer: timeout=%li\n", timeout_ms);
		instance* inst = (instance*) userdata;
		inst->timer.setInterval(timeout_ms < 0 ? 0 : timeout_ms);
		return 0;
	}
	void cb_timer(void *userdata, int count) {
		//printf("cb_timer: count=%i\n", count);
		instance* inst = (instance*) userdata;
		inst->timer.setInterval(0);
		int num_transfers;
		curl_multi_socket_action(inst->m, CURL_SOCKET_TIMEOUT, 0, &num_transfers);
		checkQueue(inst);
	}
	void newInstance(instance* inst, CP::Poll* p) {
		inst->m = curl_multi_init();
		inst->p = p;
		inst->timer.setCallback( { &cb_timer, (void*) inst });
		p->add(inst->timer);
		//event_assign(&inst->timer_event, inst->eb, -1, 0, cb_timer, inst);
		curl_multi_setopt(inst->m, CURLMOPT_SOCKETFUNCTION, cb_sock);
		curl_multi_setopt(inst->m, CURLMOPT_SOCKETDATA, inst);
		curl_multi_setopt(inst->m, CURLMOPT_TIMERFUNCTION, cb_curl_timer);
		curl_multi_setopt(inst->m, CURLMOPT_TIMERDATA, inst);
	}
	void dispose(instance* inst) {
		curl_multi_cleanup(inst->m);
	}

	void transferManager::addTransfer(const char* url, bool post,
			const function<bool(const void* data, int len, int state)>& cb) {
		if (itemsProcessing < concurrency) {
			doTransfer(url, post, cb);
		} else {
			q.push( { url, post, cb });
		}
	}
	void transferManager::checkQueue() {
		if (itemsProcessing < concurrency && q.size() > 0) {
			item& it = q.front();
			doTransfer(it.url.c_str(), it.post, it.cb);
			q.pop();
		}
	}
	void transferManager::doTransfer(const char* url, bool post,
			const function<bool(const void* data, int len, int state)>& cb) {
		itemsProcessing++;
		transferInfo* t = curl::addTransfer(&inst, url,
				[cb,this](const void* data, int len, int state)
				{
					if(state==4) {
						itemsProcessing--;
						checkQueue();
					}
					return cb(data, len, state);
				});
		if (post) curl_easy_setopt(t->c, CURLOPT_POST, 1);
		curl::beginTransfer(&inst, t);
	}
}
/*
 int main(int argc, char **argv)
 {
 curl::instance inst;
 curl::newInstance(&inst);
 curl::addTransfer(&inst,"http://192.168.5.11/",[](const void* data, int len, int state)
 {
 cout << len << endl;
 //if(data!=NULL && len>0)
 //	write(1,data,len);
 return true;
 });
 curl::eventLoop(&inst);

 return 0;
 } //*/

