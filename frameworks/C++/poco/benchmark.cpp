#include <Poco/Net/ServerSocket.h>
#include <Poco/Net/HTTPServer.h>
#include <Poco/Net/HTTPRequestHandler.h>
#include <Poco/Net/HTTPRequestHandlerFactory.h>
#include <Poco/Net/HTTPResponse.h>
#include <Poco/Net/HTTPServerRequest.h>
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/Util/ServerApplication.h>

#include <iostream>
#include <string>
#include <vector>

#define PLAIN_URL_PATH       "/plaintext"
#define PLAIN_CONTENT_TYPE   "text/plain"
#define RES_BODY             "Hello, World!"
#define SERVER_NAME          "poco"

using namespace Poco::Net;
using namespace Poco::Util;
using namespace std;

class MyRequestHandler : public HTTPRequestHandler {
public:
    virtual void handleRequest(HTTPServerRequest &req, HTTPServerResponse &resp) {
        resp.setStatusAndReason(HTTPResponse::HTTP_OK, "OK");
        resp.setContentType(PLAIN_CONTENT_TYPE);
        resp.add("Server", SERVER_NAME);
        resp.sendBuffer(RES_BODY, sizeof(RES_BODY)-1);
        return;
    }
};

class NotFoundRequestHandler : public HTTPRequestHandler {
public:
    virtual void handleRequest(HTTPServerRequest &req, HTTPServerResponse &resp) {
        resp.setStatusAndReason(HTTPResponse::HTTP_NOT_FOUND, "NOT_FOUND");
        resp.setContentType(PLAIN_CONTENT_TYPE);
        resp.add("Server", SERVER_NAME);
        resp.sendBuffer("", 0);
        return;
    }
};

class MyRequestHandlerFactory : public HTTPRequestHandlerFactory {
public:
    virtual HTTPRequestHandler* createRequestHandler(const HTTPServerRequest &req) {
        if (req.getMethod() == "GET" && req.getURI() == PLAIN_URL_PATH)
            return new MyRequestHandler;
        else
            return new NotFoundRequestHandler;
    }
};

class MyServerApp : public ServerApplication {
protected:
    int main(const vector<string> &args) {
        HTTPServerParams* hsp = new HTTPServerParams;
        hsp->setMaxThreads(stoi(args[1]));
        hsp->setKeepAlive(true);
        HTTPServer s(new MyRequestHandlerFactory, ServerSocket(stoi(args[0]), 4000), hsp);
        s.start();
        waitForTerminationRequest();
        s.stop();
        return Application::EXIT_OK;
    }
};

int main(int argc, char** argv) {
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " port nthreads" << std::endl;
        return 1;
    }
    
    MyServerApp app;
    return app.run(argc, argv);
}

