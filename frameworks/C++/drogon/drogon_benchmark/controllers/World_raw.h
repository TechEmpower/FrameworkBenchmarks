#pragma once
#include <drogon/orm/Row.h>
#include <drogon/orm/Field.h>
#include <json/json.h>

using namespace drogon::orm;
class World
{
  public:
    explicit World(const Row &r) noexcept
        : id_(r[(size_t)0].as<int32_t>()),
          randomNumber_(r[(size_t)1].as<int32_t>())
    {
    }
    inline Json::Value toJson() const
    {
        Json::Value ret;
        ret["id"] = id_;
        ret["randomnumber"] = randomNumber_;
        return ret;
    }
    inline int32_t getId() const noexcept
    {
        return id_;
    }
    inline int32_t getRandomnumber() const noexcept
    {
        return randomNumber_;
    }

  private:
    int32_t id_;
    int32_t randomNumber_;
};
