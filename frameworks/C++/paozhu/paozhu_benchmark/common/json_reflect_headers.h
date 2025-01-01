#pragma once
#include <sstream>
#include <string>
#include <vector>
#include <map>

#include "types/techempower_json.h"
#include "unicode.h"

template <typename JSON_REF_OBJ_TEMP>
std::string json_encode([[maybe_unused]] const JSON_REF_OBJ_TEMP &json_reflectobj) { return ""; }

template <typename JSON_REF_OBJ_TEMP>
std::string json_encode([[maybe_unused]] const std::vector<JSON_REF_OBJ_TEMP> &json_reflectobj) { return ""; }

template <typename JSON_REF_OBJ_TEMP>
unsigned int json_decode([[maybe_unused]] JSON_REF_OBJ_TEMP &json_reflectobj, [[maybe_unused]] const std::string &_json_data, [[maybe_unused]] unsigned int _offset) { return 0; }

template <typename JSON_REF_OBJ_TEMP>
unsigned int json_decode([[maybe_unused]] std::vector<JSON_REF_OBJ_TEMP> &json_reflectobj, [[maybe_unused]] const std::string &_json_data, [[maybe_unused]] unsigned int _offset) { return 0; }

namespace http
{

std::string json_encode(const techempower_outjson_t &json_reflectobj);

std::string json_encode(const std::vector<techempower_outjson_t> &json_reflectobj);

unsigned int json_decode(techempower_outjson_t &json_reflectobj, const std::string &_json_data, unsigned int _offset = 0);

unsigned int json_decode(std::vector<techempower_outjson_t> &json_reflectobj, const std::string &_json_data, unsigned int _offset = 0);
}// namespace http
