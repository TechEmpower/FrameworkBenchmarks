#include <iostream>
#include <memory>
#include <string_view>

#include "orm.h"
#include "httppeer.h"
#include "websockets.h"
#include "terminal_color.h"
// g++ -shared -fPIC mywebsockets.cpp -o mywebsockets.so
namespace http
{

class mywebsockets : public websockets_api
{
  public:
    //    unsigned int timeloop_num;
    //    unsigned char state;
    unsigned int outcount = 0;
    mywebsockets(std::weak_ptr<httppeer> p) : websockets_api(4, 0, p) {}
    ~mywebsockets() { DEBUG_LOG(" ~mywebsockets "); }

  public:
    void onopen() { DEBUG_LOG(" onopen "); }
    void onclose() { DEBUG_LOG(" onclose "); }
    void onpong() {}
    void pushloop()
    {
        std::shared_ptr<httppeer> peer = weak_peer.lock();
        if (peer)
        {
            DEBUG_LOG(" timeloop ");
            std::string aa = "This server push msg or subscribe msg";
            std::string outhello;
            peer->ws->makeWSText(aa.data(), aa.length(), outhello);
            peer->send(outhello);

            //   peer->send(aa);
            if (outcount == 4)
            {
                timeloop_num = 0;
                outcount     = 0;
                return;
            }
            outcount++;
        }
        else
        {
            DEBUG_LOG(" peer is die! ");
        }
    }

    void onfiles([[maybe_unused]] std::string_view filename) { DEBUG_LOG("onfiles %zu", filename.size()); }
    void onmessage(std::string_view data)
    {
        std::ostringstream oss;
        oss << std::this_thread::get_id();
        oss << " onmessage:" << data << std::endl;
        std::string temp = oss.str();
        DEBUG_LOG("%s", temp.c_str());
        std::shared_ptr<http::httppeer> peer = weak_peer.lock();
        if (peer)
        {
            std::string outhello;
            if(data=="html")
            {
               std::string html_data="<h3> Websocket test 测试h3 </h3>";
               peer->ws->makeWSText(html_data, outhello); 
            }
            else
            {
                peer->ws->makeWSText(data, outhello);
            }
            
            peer->send(outhello);
        }
    }
    static std::shared_ptr<mywebsockets> create(std::weak_ptr<http::httppeer> p)
    {
        return std::make_shared<mywebsockets>(p);
    }
};

}// namespace http