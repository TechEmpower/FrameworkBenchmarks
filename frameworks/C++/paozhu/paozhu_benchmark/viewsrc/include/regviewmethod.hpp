#ifndef __HTTP_REG_VIEW_METHOD_HPP
#define __HTTP_REG_VIEW_METHOD_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif // defined(_MSC_VER) && (_MSC_VER >= 1200)

#include<string>
#include<map>
#include<functional>
#include "request.h"
#include "viewso_param.h"
#include "viewmethold_reg.h"
#include "viewsrc.h"

namespace http
{
  void _initview_method_regto(VIEW_REG  &_viewmetholdreg)
  {
            	 //create time: Mon, 30 Dec 2024 15:45:45 GMT

	_viewmetholdreg.emplace("techempower/fortunes",http::view::techempower::fortunes);

	} 
}
#endif