#ifndef ORM_DEFAULT_WORLD_H
#define ORM_DEFAULT_WORLD_H
#include "world_mysql.h" 
#include "worldbase.h"

/* 如果此文件存在不会自动覆盖，没有则会自动生成。
*If this file exists, it will not be overwritten automatically. If not, it will be generated automatically. */

 namespace orm {
		class World : public world_mysql<World,worldbase>{
		 public:
		 World(std::string dbtag);
		 World();
		};
}
#endif
