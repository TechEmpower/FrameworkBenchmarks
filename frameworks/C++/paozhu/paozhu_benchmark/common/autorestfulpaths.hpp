
#ifndef __HTTP_AUTO_REG_CONTROL_HTTPRESTFUL_HPP
#define __HTTP_AUTO_REG_CONTROL_HTTPRESTFUL_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif // defined(_MSC_VER) && (_MSC_VER >= 1200)

#include "httppeer.h" 



namespace http
{
  void _initauto_control_httprestful_paths(std::map<std::string, std::vector<std::string>>  &restfulmethod)
  {
    

        if(restfulmethod.size())
        {}
        

  }
    
    void _initauto_domain_httprestful_paths(std::map<std::string,std::map<std::string, std::vector<std::string>>>  &restfulmethod)
    {
        std::map<std::string, std::vector<std::string>> temp_path;
        std::map<std::string,std::map<std::string, std::vector<std::string>>>::iterator domain_iterator;  

        domain_iterator=restfulmethod.begin();
        temp_path.clear();
        

    }
    
}

#endif

    