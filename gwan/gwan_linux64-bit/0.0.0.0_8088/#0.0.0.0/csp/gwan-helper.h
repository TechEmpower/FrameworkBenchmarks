#include "gwan.h"
#include <string>
#include <sstream> // inefficient, but more readable
#include <iostream>
using namespace std;


class Http {
  protected:
    int argc;
    char **argv;
    xbuf_t *reply;
    char *server_date;
    ostringstream content_buffer;
    string content_type;
    string connection_type;
    bool changed = false;
  public:
    Http(int argc,char**args) : argc(argc), argv(args) {
      reply = get_reply(argv);
      server_date = (char*)get_env(argv, SERVER_DATE);
      connection_type = "keep-alive";
    }
    void set_connection_type(const char type[]) {
      connection_type = type;
      changed = true;
    }
    void set_content_type(const char type[]) {
      content_type = type;
      changed = true;
    }
    int get_i(const char key[]) {
      char *str = get_s(key);
      if(!str) return 0;
      return atoi(str);
    }
    char *get_s(const char key[]) {
      char *str = 0;
      get_arg((char*)key,&str,argc,argv);
      return str;
    }
    int end(int code=200) {
      if(!changed) {
        xbuf_cat(reply, (char*) content_buffer.str().c_str());
        return code;
      }
      const char *code_text = 0;
      switch(code) {
        case 200: code_text = "200 OK"; break;
        case 301: code_text = "301 Moved Permanently"; break;
        case 403: code_text = "403 Forbidden"; break;
        case 404: code_text = "404 Not Found"; break;
        case 500: code_text = "500 Internal Server Error"; break;
        // TODO: add more
        default: code_text = "501 Not Implemented";
      }
      ostringstream header;
      header
          << "HTTP/1.1 " << code_text << "\r\n"
          << "Date: " << server_date << "\r\n"
          << "Last-Modified: " << server_date << "\r\n"
          << "Server: G-WAN" << "\r\n"
          << "Content-Type: " << content_type << "\r\n"
          << "Content-Length: " << content_buffer.tellp() << "\r\n"
          << "\r\n";
      xbuf_cat(reply,(char*) header.str().c_str());
      xbuf_cat(reply,(char*) content_buffer.str().c_str());
      return code;
    }

    ~Http() {

    }

    template<typename T> friend Http& operator<<(Http&,const T&);
};

template<typename T>
Http& operator<<(Http& h,const T &content) {
  h.content_buffer << content;
  return h;
}

// xml::escape
// source: http://stackoverflow.com/questions/7976445/so-weve-got-our-html-escape-functions-that-really-work-in-a-c-manner-how-to

#include <algorithm>

namespace xml {

    // Helper for null-terminated ASCII strings (no end of string iterator).
    template<typename InIter, typename OutIter>
    OutIter copy_asciiz ( InIter begin, OutIter out )
    {
        while ( *begin != '\0' ) {
            *out++ = *begin++;
        }
        return (out);
    }

    // XML escaping in it's general form.  Note that 'out' is expected
    // to an "infinite" sequence.
    template<typename InIter, typename OutIter>
    OutIter escape ( InIter begin, InIter end, OutIter out )
    {
        static const char bad[] = "&<>";
        static const char* rep[] = {"&amp;", "&lt;", "&gt;"};
        static const std::size_t n = sizeof(bad)/sizeof(bad[0]);

        for ( ; (begin != end); ++begin )
        {
            // Find which replacement to use.
            const std::size_t i =
                std::distance(bad, std::find(bad, bad+n, *begin));

            // No need for escaping.
            if ( i == n ) {
                *out++ = *begin;
            }
            // Escape the character.
            else {
                out = copy_asciiz(rep[i], out);
            }
        }
        return (out);
    }

}

#include <iterator>
#include <string>

namespace xml {

    // Get escaped version of "content".
    std::string escape ( const std::string& content )
    {
        std::string result;
        result.reserve(content.size());
        escape(content.begin(), content.end(), std::back_inserter(result));
        return (result);
    }

    // Escape data on the fly, using "constant" memory.
    void escape ( std::istream& in, std::ostream& out )
    {
        escape(std::istreambuf_iterator<char>(in),
            std::istreambuf_iterator<char>(),
            std::ostreambuf_iterator<char>(out));
    }

}
