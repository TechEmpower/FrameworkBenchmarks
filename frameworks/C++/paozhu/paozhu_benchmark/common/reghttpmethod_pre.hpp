#ifndef __HTTP_REGHTTPMETHOD_PRE_HPP
#define __HTTP_REGHTTPMETHOD_PRE_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif// defined(_MSC_VER) && (_MSC_VER >= 1200)

#include "httppeer.h"
namespace http
{
void _inithttpmethodregto_pre(std::map<std::string, regmethold_t> &methodcallback)
{
    struct regmethold_t temp;
    //temp.pre = adminlogin;
    //temp.regfun = adminmar;
    //methodcallback.emplace("adminmar", temp);
}

}// namespace http
#endif