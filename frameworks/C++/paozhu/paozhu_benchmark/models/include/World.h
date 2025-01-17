#ifndef ORM_DEFAULT_WORLD_H
#define ORM_DEFAULT_WORLD_H
#include "mysqlorm.hpp"
#include "worldbase.h"

/* 如果此文件存在不会自动覆盖，没有则会自动生成。
 *If this file exists, it will not be overwritten automatically. If not, it will be generated automatically. */

namespace orm
{
class World : public mysql_orm<World, worldbase>
{
  public:
    World(std::string dbtag);
    World();
};
};// namespace orm
#endif
