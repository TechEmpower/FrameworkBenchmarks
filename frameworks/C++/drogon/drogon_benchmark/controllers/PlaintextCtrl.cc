#include "PlaintextCtrl.h"
void PlaintextCtrl::asyncHandleHttpRequest(
    const HttpRequestPtr &req,
    std::function<void(const HttpResponsePtr &)> &&callback)
{
    auto resp = HttpResponse::newHttpResponse();
    resp->setBody("Hello, World!");
    resp->setContentTypeCodeAndCustomString(CT_TEXT_PLAIN,
                                            "Content-Type: text/plain\r\n");
    //    resp->setExpiredTime(0);
    callback(resp);
}
