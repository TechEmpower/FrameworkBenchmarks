#ifndef ORM_DEFAULT_FORTUNE_H
#define ORM_DEFAULT_FORTUNE_H
#include "mysqlmodel.hpp" 
#include "fortunebase.h"

/* 如果此文件存在不会自动覆盖，没有则会自动生成。
*If this file exists, it will not be overwritten automatically. If not, it will be generated automatically. */

 namespace orm {
		class Fortune : public mysqlclientDB<Fortune,fortunebase>{
		 public:
		 Fortune(std::string dbtag);
		 Fortune();
		};
};
#endif
