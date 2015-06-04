
#include <json/json.h>

class JsonObj {
  protected:
    json_object *data;
  public:
    JsonObj() {
      data=json_object_new_object();
    }
    void add(const char key[], const char value[]) {
      json_object_object_add(data, key, json_object_new_string(value));
    }
    void add(const char key[], int value) {
      json_object_object_add(data, key, json_object_new_int(value));
    }
    char* to_s() {
      return (char*)json_object_to_json_string(data);
    }
    virtual ~JsonObj() {
      json_object_put(data);
    }
};
