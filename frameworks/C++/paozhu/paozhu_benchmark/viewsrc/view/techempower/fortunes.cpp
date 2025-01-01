#include<iostream>
#include <cstdio>
#include <string>
#include <sstream>
#include <map> 
#include <vector>
#include <ctime>
#include <array>
#include <sys/stat.h>
#include <cstdlib>
#include "request.h"
#include "datetime.h"
#include "cookie.h"
#include "urlcode.h"
#include "loadviewso.h"
#include "viewso_param.h"
#include "http_so_common_api.h"
#include "viewsrc.h"
//g++ viewsrc/view/techempower/fortunes.cpp
namespace http {

namespace view {
	namespace techempower{
 		 std::string fortunes([[maybe_unused]] const struct view_param &vinfo,[[maybe_unused]] http::OBJ_VALUE &obj)
			{
 
                     std::ostringstream echo;

        
 			 echo<<"<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";

    for(auto &a:obj["list"].as_array()){

 			 echo<<"<tr><td>";
 echo<<a.second["id"].to_string(); 
 			 echo<<"</td><td>";
 echo<<a.second["message"].to_string(); 
 			 echo<<"</td></tr>";

    }

 			 echo<<"</table></body></html>";

                  return echo.str();
             }

         }
        }
       }
    