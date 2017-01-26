/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012-2014, Eduardo Silva P. <edsiper@gmail.com>.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

/*
 * @OBJ_NAME: json
 * @OBJ_MENU: JSON
 * @OBJ_DESC: The JSON package expose differnet methods to compose or parse
 * JSON data.
 * @PKG_HEADER: #include "packages/json/json.h"
 * @PKG_INIT: duda_load_package(json, "json");
 */

#include "duda_package.h"
#include "json.h"

/*
 * @METHOD_NAME: create_null
 * @METHOD_DESC: It creates a NULL JSON item type
 * @METHOD_PROTO: json_t *create_null()
 * @METHOD_RETURN: Returns a NULL type JSON item.
 */
json_t *json_create_null()
{
    return cJSON_CreateNull();
}

/*
 * @METHOD_NAME: create_true
 * @METHOD_DESC: It creates a True JSON item type
 * @METHOD_PROTO: json_t *create_true()
 * @METHOD_RETURN: Returns a True type JSON item.
 */
json_t *json_create_true()
{
    return cJSON_CreateTrue();
}

/*
 * @METHOD_NAME: create_false
 * @METHOD_DESC: It creates a False JSON item type
 * @METHOD_PROTO: json_t *create_false()
 * @METHOD_RETURN: Returns a False type JSON item.
 */
json_t *json_create_false()
{
    return cJSON_CreateFalse();
}

/*
 * @METHOD_NAME: create_bool
 * @METHOD_DESC: It creates a Boolean JSON item type
 * @METHOD_PROTO: json_t *create_bool(int bool)
 * @METHOD_PARAM: bool the boolean value, 0 = False, 1 = True
 * @METHOD_RETURN: Returns a Boolean type JSON item.
 */
json_t *json_create_bool(int bool)
{
    return cJSON_CreateBool(bool);
}

/*
 * @METHOD_NAME: create_number
 * @METHOD_DESC: It creates a Numeric JSON item type
 * @METHOD_PROTO: json_t *create_number(double number)
 * @METHOD_PARAM: number The numeric value
 * @METHOD_RETURN: Returns a Numeric type JSON item.
 */
json_t *json_create_number(double number)
{
    return cJSON_CreateNumber(number);
}

/*
 * @METHOD_NAME: create_string
 * @METHOD_DESC: It creates a String JSON item type
 * @METHOD_PROTO: json_t *create_string(const char *string)
 * @METHOD_PARAM: string the string data for the item
 * @METHOD_RETURN: Returns a String JSON item type.
 */
json_t *json_create_string(const char *string)
{
    return cJSON_CreateString(string);
}

/*
 * @METHOD_NAME: create_array
 * @METHOD_DESC: It creates an Array JSON item type
 * @METHOD_PROTO: json_t *create_array()
 * @METHOD_RETURN: Returns an Array JSON item type.
 */
json_t *json_create_array()
{
    return cJSON_CreateArray();
}

/*
 * @METHOD_NAME: create_object
 * @METHOD_DESC: It creates an Object JSON item type
 * @METHOD_PROTO: json_t *create_object()
 * @METHOD_RETURN: Returns an Object JSON item type.
 */
json_t *json_create_object()
{
    return cJSON_CreateObject();
}

/*
 * @METHOD_NAME: add_to_array
 * @METHOD_DESC: It appends an item to the given array
 * @METHOD_PROTO: void add_to_array(json_t *array, json_t *item)
 * @METHOD_PARAM: array the JSON target array
 * @METHOD_PARAM: item the JSON item to be added
 * @METHOD_RETURN: None
 */
void json_add_to_array(json_t *array, json_t *item)
{
    return cJSON_AddItemToArray(array, item);
}

/*
 * @METHOD_NAME: add_to_object
 * @METHOD_DESC: It links an item to an object through a key
 * @METHOD_PROTO: void add_to_object(json_t *object, const char *key, json_t *item)
 * @METHOD_PARAM: object the JSON target object
 * @METHOD_PARAM: key the key name for the item
 * @METHOD_PARAM: item the JSON item to be added
 * @METHOD_RETURN: None
 */
void json_add_to_object(json_t *object, const char *key, json_t *item)
{
    return cJSON_AddItemToObject(object, key, item);
}

/*
 * @METHOD_NAME: parse
 * @METHOD_DESC: Parse a JSON string, creates a new root and populate
 * @METHOD_PROTO: json_t *parse(const char *data)
 * @METHOD_RETURN: Returns the JSON root object
 */
json_t *json_parse(const char *data)
{
    return cJSON_Parse(data);
}

/*
 * @METHOD_NAME: print
 * @METHOD_DESC: Render a JSON item, entity or structure to formatted text. The
 * memory allocated for the buffer must be released with an explicit free(2). If
 * the buffer will be send as a response we suggest you use print_gc() version
 * instead.
 * @METHOD_PROTO: char *print(json_t *item)
 * @METHOD_PARAM: item the JSON item to be rendered
 * @METHOD_RETURN: Returns the rendered text string (formatted)
 */
char *json_print(json_t *item)
{
    return cJSON_Print(item);
}

/*
 * @METHOD_NAME: print_gc
 * @METHOD_DESC: Render a JSON item, entity or structure to formatted text, the
 * memory allocated for the text is released later by the Garbage Collector.
 * @METHOD_PROTO: char *print_gc(duda_request_t *dr, json_t *item)
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: item the JSON item to be rendered
 * @METHOD_RETURN: Returns the rendered text string (formatted)
 */

char *json_print_gc(duda_request_t *dr, json_t *item)
{
    char *buf = NULL;

    buf = cJSON_Print(item);
    if (buf) {
        gc->add(dr, buf);
    }

    return buf;
}

/*
 * @METHOD_NAME: print_unformatted
 * @METHOD_DESC: Render a JSON item, entity or structure to unformatted text. The
 * memory allocated for the buffer must be released with an explicit free(2). If
 * the buffer will be send as a response we suggest you use print_unformatted_gc() version
 * instead.
 * @METHOD_PROTO: char *print_unformatted(json_t *item)
 * @METHOD_PARAM: item the JSON item to be rendered
 * @METHOD_RETURN: Returns the rendered text string (un-formatted)
 */

char *json_print_unformatted(json_t *item)
{
    return cJSON_PrintUnformatted(item);
}

/*
 * @METHOD_NAME: print_unformatted_gc
 * @METHOD_DESC: Render a JSON item, entity or structure to unformatted text. The
 * memory allocated for the text is released later by the Garbage Collector.
 * @METHOD_PROTO: char *print_unformatted(duda_request_t *dr, json_t *item)
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: item the JSON item to be rendered
 * @METHOD_RETURN: Returns the rendered text string (un-formatted)
 */
char *json_print_unformatted_gc(duda_request_t *dr, json_t *item)
{
    char *buf = NULL;

    buf = cJSON_PrintUnformatted(item);
    if (buf) {
        gc->add(dr, buf);
    }

    return buf;
}

/*
 * @METHOD_NAME: delete
 * @METHOD_DESC: Delete a JSON structure
 * @METHOD_PROTO: void delete(json_t *item)
 * @METHOD_PARAM: item the item tree to be deleted
 * @METHOD_RETURN: None
 */
void json_delete(json_t *item)
{
    cJSON_Delete(item);
}

/*
 * @METHOD_NAME: get_array_size
 * @METHOD_DESC: Get the array size in terms of number of items
 * @METHOD_PROTO: int get_array_size(json_t *array)
 * @METHOD_PARAM: array The JSON array
 * @METHOD_RETURN: Returns the number of items in the array
 */
int json_get_array_size(json_t *array)
{
    return cJSON_GetArraySize(array);
}

/*
 * @METHOD_NAME: get_array_item
 * @METHOD_DESC: Get an array item based on its position
 * @METHOD_PROTO: json_t *get_array_item(json_t *array, int n)
 * @METHOD_PARAM: array The JSON array
 * @METHOD_PARAM: n     The item position inside the array
 * @METHOD_RETURN: Returns the array item
 */
json_t *json_get_array_item(json_t *array, int n)
{
    return cJSON_GetArrayItem(array, n);
}

/*
 * @METHOD_NAME: get_object_item
 * @METHOD_DESC: Get an object item based on its key name
 * @METHOD_PROTO: json_t *get_object_item(json_t *object, const char *key)
 * @METHOD_PARAM: object The JSON object
 * @METHOD_PARAM: key The key name of the object item
 * @METHOD_RETURN: Returns the object item
 */
json_t *json_get_object_item(json_t *object, const char *key)
{
    return cJSON_GetObjectItem(object, key);
}

/*
 * @METHOD_NAME: get_error
 * @METHOD_DESC: Get the error string from an exception
 * @METHOD_PROTO: const char *get_error()
 * @METHOD_RETURN: Returns the text error
 */
const char *json_get_error()
{
    return cJSON_GetErrorPtr();
}

/* Local methods */

/*
 * @METHOD_NAME: parse_data
 * @METHOD_DESC: Based in a POST request with a JSON body, it parse the
 * content and returns the root JSON object.
 * @METHOD_PROTO: json_t *parse_data(duda_request_t *dr)
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_RETURN: Returns the JSON root object
 */
json_t *json_parse_data(duda_request_t *dr)
{
    unsigned long len;
    json_t *root;
    char *data;

    /* Check that we have a post request with some content */
    if (!request->is_data(dr)) {
        return NULL;
    }

    /* Generate a local copy of the data */
    data = request->get_data(dr, &len);
    if (!data) {
        return NULL;
    }

    /* Parse JSON request */
    root = cJSON_Parse(data);

    /* Free the copied data */
    monkey->mem_free(data);

    /* return the JSON root tree */
    return root;
}

/* API object */
struct duda_api_json *get_json_api()
{
    struct duda_api_json *json;

    /* Alloc object */
    json = monkey->mem_alloc(sizeof(struct duda_api_json));

    /* Map API calls */
    json->create_null          = json_create_null;
    json->create_true          = json_create_true;
    json->create_false         = json_create_false;
    json->create_bool          = json_create_bool;
    json->create_number        = json_create_number;
    json->create_string        = json_create_string;
    json->create_array         = json_create_array;
    json->create_object        = json_create_object;

    json->add_to_array         = json_add_to_array;
    json->add_to_object        = json_add_to_object;

    json->parse                = json_parse;
    json->print                = json_print;
    json->print_gc             = json_print_gc;
    json->print_unformatted    = json_print_unformatted;
    json->print_unformatted_gc = json_print_unformatted_gc;
    json->delete               = json_delete;
    json->get_array_size       = json_get_array_size;
    json->get_array_item       = json_get_array_item;
    json->get_object_item      = json_get_object_item;
    json->get_error            = json_get_error;

    /* Local methods */
    json->parse_data           = json_parse_data;

    return json;
}

duda_package_t *duda_package_main()
{
    duda_package_t *dpkg;

    /* Package object */
    dpkg = monkey->mem_alloc(sizeof(duda_package_t));
    dpkg->name = "json";
    dpkg->version = "0.1";
    dpkg->api = get_json_api();

    return dpkg;
}
