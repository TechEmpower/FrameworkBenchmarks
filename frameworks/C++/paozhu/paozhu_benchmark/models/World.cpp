#include "mysqlmodel.hpp" 
#include "worldbase.h"
#include "World.h"

/* 如果此文件存在不会自动覆盖，没有则会自动生成。
*If this file exists, it will not be overwritten automatically. If not, it will be generated automatically. */

	 
 namespace orm{
 

			 World::World(std::string dbtag):mysqlclientDB(dbtag){}
			 World::World():mysqlclientDB(){}


	  }
