#pragma once
#include <map>
#include "httppeer.h"
#include "websockets.h"
#include "mywebsockets.hpp"
#include "websockets_callback.h"
namespace http
{
void _initwebsocketmethodregto(WEBSOCKET_REG &methodcallback)
{

    methodcallback.emplace("wstest", [](std::weak_ptr<httppeer> p) -> std::shared_ptr<websockets_api>
                           { return http::mywebsockets::create(p); });
    //    methodcallback.emplace("looptest",[](std::weak_ptr<clientpeer> p)->std::shared_ptr<websockets_api>{
    //      return http::loopwebsockets::create(p);
    //   });
}

}// namespace http