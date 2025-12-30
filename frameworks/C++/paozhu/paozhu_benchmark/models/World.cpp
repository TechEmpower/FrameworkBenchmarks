
#include "world_mysql.h"
#include "world_base.h"
#include "World.h"

/* 如果此文件存在不会自动覆盖，没有则会自动生成。
*If this file exists, it will not be overwritten automatically. If not, it will be generated automatically. */

	 
 namespace orm{
 
			 World::World(std::string dbtag):world_mysql(dbtag){ mod=this; }
			 World::World():world_mysql(){ mod=this; }


	  }
