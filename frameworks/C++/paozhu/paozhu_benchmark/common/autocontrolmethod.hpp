
#ifndef __HTTP_AUTO_REG_CONTROL_HTTPMETHOD_HPP
#define __HTTP_AUTO_REG_CONTROL_HTTPMETHOD_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif // defined(_MSC_VER) && (_MSC_VER >= 1200)

#include "httppeer.h" 

#include "techempower.h"


namespace http
{
  void _initauto_control_httpmethodregto(std::map<std::string, regmethold_t> &methodcallback)
  {
    struct regmethold_t temp;

		temp.pre = nullptr;
		temp.regfun = techempowerplaintext;
		methodcallback.emplace("plaintext",temp);
		temp.pre = nullptr;
		temp.regfun = techempowerjson;
		methodcallback.emplace("json",temp);
		temp.pre = nullptr;
		temp.regfun = techempowerdb;
		methodcallback.emplace("db",temp);
		temp.pre = nullptr;
		temp.regfun = techempowerqueries;
		methodcallback.emplace("queries",temp);
		temp.pre = nullptr;
		temp.regfun = techempowerfortunes;
		methodcallback.emplace("fortunes",temp);
		temp.pre = nullptr;
		temp.regfun = techempowerupdates;
		methodcallback.emplace("updates",temp);
		temp.pre = nullptr;
		temp.regfun = techempowercached_queries;
		methodcallback.emplace("cached-queries",temp);
		temp.pre = nullptr;
		temp.regfun = techempowercached_db;
		methodcallback.emplace("cached-db",temp);


    }
}

#endif

    