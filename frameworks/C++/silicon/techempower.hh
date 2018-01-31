#include <unistd.h>
#include <iostream>
#include <silicon/api.hh>
#include <silicon/middleware_factories.hh>
#include <silicon/middlewares/mysql_connection.hh>
#include <silicon/middlewares/mysql_orm.hh>
#include "symbols.hh"

using namespace s;
using namespace sl;

typedef decltype(D(_id(_auto_increment, _primary_key) = int(),
                   _randomNumber = int())) random_number;

typedef decltype(D(_id(_auto_increment, _primary_key) = int(),
                   _message = std::string())) fortune;

typedef mysql_orm_factory<random_number> rn_orm_factory;
typedef mysql_orm<random_number> rn_orm;

typedef mysql_orm_factory<fortune> fortune_orm_factory;
typedef mysql_orm<fortune> fortune_orm;


std::string escape_html_entities(std::string& data)
{
    std::string buffer;
    buffer.reserve(data.size());
    for(size_t pos = 0; pos != data.size(); ++pos) {
        switch(data[pos]) {
            case '&':  buffer.append("&amp;");       break;
            case '\"': buffer.append("&quot;");      break;
            case '\'': buffer.append("&apos;");      break;
            case '<':  buffer.append("&lt;");        break;
            case '>':  buffer.append("&gt;");        break;
            default:   buffer.append(&data[pos], 1); break;
        }
    }
    return std::move(buffer);
}

auto techempower_api = http_api(

  GET / _plaintext = [] () { return response(_content_type = string_ref("text/plain"),
                                       _body = string_ref("Hello, World!")); },

  GET / _json = [] () { return response(_content_type = string_ref("application/json"),
                                  _body = D(_message = "Hello, World!")); },
                        
  GET / _db = [] (rn_orm& orm) {
    random_number r;
    orm.find_by_id(1245, r);
    return response(_content_type = "application/json",
                    _body = r);
  },

  GET / _queries * get_parameters(_queries = optional(std::string("1"))) = [] (auto param, rn_orm& orm) {
    int N = atoi(param.queries.c_str());
    N = std::max(1, std::min(N, 500));

    std::vector<random_number> qs(N);
    for (int i = 0; i < N; i++)
      orm.find_by_id(1 + rand() % 9999, qs[i]);
    return response(_content_type = "application/json",
                    _body = std::move(qs));
  },

  GET / _updates * get_parameters(_queries = optional(std::string("1"))) = [] (auto param, rn_orm& orm) {
    int N = atoi(param.queries.c_str());
    N = std::max(1, std::min(N, 500));

    std::vector<random_number> qs(N);
    for (int i = 0; i < N; i++)
    {
      orm.find_by_id(1 + rand() % 9999, qs[i]);
      qs[i].randomNumber = 1 + rand() % 9999;
      orm.update(qs[i]);
    }
    return response(_content_type = "application/json",
                    _body = std::move(qs));
  },
  
  GET / _fortunes = [] (fortune_orm& orm) {
    std::vector<fortune> table;
    orm.forall([&] (fortune& f) { table.push_back(f); });
    table.push_back(fortune(0, "Additional fortune added at request time."));

    std::sort(table.begin(), table.end(),
              [] (const fortune& a, const fortune& b) { return a.message < b.message; });

    std::stringstream ss;

    ss << "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
    for(auto& f : table)
      ss << "<tr><td>" << f.id << "</td><td>" << escape_html_entities(f.message) << "</td></tr>";
    ss << "</table></body></html>";

    return response(_content_type = "text/html; charset=utf-8",
                    _body = ss.str());
  }
  
  );
