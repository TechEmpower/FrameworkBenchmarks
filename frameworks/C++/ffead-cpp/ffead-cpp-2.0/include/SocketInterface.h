/*
 * SocketInterface.h
 *
 *  Created on: 02-Jan-2015
 *      Author: sumeetc
 */

#ifndef SOCKETINTERFACE_H_
#define SOCKETINTERFACE_H_
#include "SocketUtil.h"
#include "Timer.h"
#include "Mutex.h"

class SocketInterface {
	friend class RequestReaderHandler;
	friend class ServiceHandler;
	friend class HandlerRequest;
protected:

	SocketUtil* sockUtil;
	string buffer;
	Timer t;
	int fd;
	bool write(const string& data)
	{
		int offset = 0;
		while(!isClosed() && offset<(int)data.length())
		{
			//cout << "writing data " << fd << " " << identifier << endl;
			int ret = sockUtil->writeData(data, true, offset);
			//cout << "done writing data " << fd << " " << identifier << endl;
			if(ret==0)
			{
				break;
			}
			else if(ret>0)
			{
				offset += ret;
			}
		}
		return isClosed();
	}
	bool read()
	{
		while (!isClosed())
		{
			ssize_t count;
			string temp;
			count = sockUtil->readData(MAXBUFLENM, temp);
			if(count>0)
			{
				t.start();
				buffer.append(temp);
			}
			else if (count == -1 && (errno == EAGAIN || errno == EINTR))
			{
				/* If errno == EAGAIN, that means we have read all
				 data. So go back to the main loop. */
				//if (errno == EAGAIN)
				//{
					break;
				//}
			}
			else if (count == 0)
			{
				/* End of file. The remote has closed the
				 connection. */
				break;
			}
			else
			{
				close();
				break;
			}
		}
		return isClosed();
	}
	void close() {
		sockUtil->closeSocket();
	}public:
	int getDescriptor() {
		return fd;
	}
long identifier;
	virtual string getProtocol(void* context)=0;
	virtual int getTimeout()=0;
	virtual void* readRequest(void*& context, int& pending)=0;
	virtual bool writeResponse(void* req, void* res, void* context)=0;
	virtual void onOpen()=0;
	virtual void onClose()=0;
	virtual ~SocketInterface() {}
	bool isClosed() {
		sockUtil->lock.lock();
		bool fl = sockUtil->closed;
		sockUtil->lock.unlock();
		return fl;
	}
	void setIdentifier(const long& identifier) {
		this->identifier = identifier;
		fd = sockUtil->fd;
	}
};

#endif /* SOCKETINTERFACE_H_ */
