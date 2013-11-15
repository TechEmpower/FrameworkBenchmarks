// Compile it with: 
//   $ gcc -o bench bench.c -lonion -Wall -O2 -lsqlite3
//   $ export ONION_LOG=noinfo  # To properly test, you may need some control over logging. Here almost disabled. 
//   $ ./bench
// It listens to localhost:8080, known addresses: http://localhost:8080/ , http://localhost:8080/db , http://localhost:8080/db20
// Test it with ab:
//   $ ab -k -t 10 -c 20 http://localhost:8080/
// It gave me (Intel(R) Core(TM) i7-2677M CPU @ 1.80GHz):
//   Requests per second:    58288.10 [#/sec] (mean)

// Done in response of http://www.techempower.com/blog/2013/03/28/framework-benchmarks/
// Although onion is not a framework.

// Copyright (c) 2013, David Moreno
// Under BSD license

// All rights reserved. 
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met: 
// 
// 1. Redistributions of source code must retain the above copyright notice, this
//    list of conditions and the following disclaimer. 
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution. 
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// 
// The views and conclusions contained in the software and documentation are those
// of the authors and should not be interpreted as representing official policies, 
// either expressed or implied, of the FreeBSD Project.



#include <onion/onion.h>
#include <onion/handler.h>
#include <onion/dict.h>
#include <onion/block.h>
#include <onion/log.h>
#include <string.h>
#include <mysql/mysql.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <json/json.h>
#include <semaphore.h>
#include <pthread.h>
#include <signal.h>
#include <malloc.h>

/// Gets the dict and converts it to JSON and writes it into the response. 
onion_connection_status return_json(onion_dict *json, onion_request *req, onion_response *res){
	onion_block *bl=onion_dict_to_json(json);
	size_t size=onion_block_size(bl);
	onion_response_set_header(res, "Content-Type","application/json");
	onion_response_set_length(res, size);
	onion_response_write(res, onion_block_data(bl), size);
	onion_block_free(bl);
	return OCS_PROCESSED;
}

/// Gets the dict and converts it to JSON and writes it into the response. 
onion_connection_status return_json_libjson(void *_, onion_request *req, onion_response *res){
	json_object *hello=json_object_new_object();
	json_object_object_add(hello, "message", json_object_new_string("Hello, world"));
	
	const char *hello_str=json_object_to_json_string(hello);
	int size=strlen(hello_str);

	onion_response_set_header(res, "Content-Type","application/json");
	onion_response_set_length(res, size);
	onion_response_write(res, hello_str, size);
	json_object_put(hello);
	return OCS_PROCESSED;
}

/// Do one sqlite petition
onion_connection_status return_db(MYSQL *db, onion_request *req, onion_response *res){
	char query[256];
	char *error;
	const char *nqueries_str=onion_request_get_query(req,"queries");
	int queries=(nqueries_str) ? atoi(nqueries_str) : 1;

	json_object *json=json_object_new_object();
	json_object *array=json_object_new_array();
	int i;
	for (i=0;i<queries;i++){
		json_object *obj=json_object_new_object();
		
		snprintf(query,sizeof(query), "SELECT * FROM World WHERE id = %d", 1 + (rand()%10000));
		mysql_query(db, query);
		MYSQL_RES *sqlres = mysql_store_result(db);
		MYSQL_ROW row = mysql_fetch_row(sqlres);
		
		json_object_object_add(obj, "randomNumber", json_object_new_int( atoi(row[1]) ));
		json_object_array_add(array, obj);
		mysql_free_result(sqlres);
	}
	json_object_object_add(json,"json",array);
	const char *str=json_object_to_json_string(json);
	int size=strlen(str);
	onion_response_set_header(res,"Content-Type","application/json");
	onion_response_set_length(res, size);
	onion_response_write(res, str, size);
	
	json_object_put(json);
	return OCS_PROCESSED;
}

onion_connection_status fortunes_html_template(onion_dict *context, onion_request *req, onion_response *res);

typedef struct fortune{
	char id[10];
	char message[2048];
}fortune_t;

typedef struct fortune_list{
	int count;
	int size;
	fortune_t *list;
}fortune_list_t;

int cmp_fortune(fortune_t *a, fortune_t *b){
	return strcmp(a->message, b->message);
}

onion_connection_status return_fortune(MYSQL *db, onion_request *req, onion_response *res){
	mysql_query(db, "SELECT id, message FROM Fortune;");
	MYSQL_RES *sqlres = mysql_store_result(db);
	if (!sqlres)
		return OCS_INTERNAL_ERROR;
	MYSQL_ROW row;
	fortune_list_t fortune_list;
	
	fortune_list.count=0;
	fortune_list.size=16;
	fortune_list.list=calloc(16,sizeof(fortune_t));
	
	while( (row=mysql_fetch_row(sqlres)) ){
		if (fortune_list.count>=fortune_list.size){
			fortune_list.size+=fortune_list.size;
			fortune_list.list=realloc(fortune_list.list, fortune_list.size * sizeof(fortune_list.size));
		}
		strncpy(fortune_list.list[fortune_list.count].id,row[0],sizeof(fortune_list.list[fortune_list.count].id));
		strncpy(fortune_list.list[fortune_list.count].message,row[1],sizeof(fortune_list.list[fortune_list.count].message));
		fortune_list.count++;
	}
	
	qsort(fortune_list.list, fortune_list.count, sizeof(fortune_t), (__compar_fn_t)cmp_fortune);
	
	onion_dict *context=onion_dict_new();
	
	onion_dict_add(context, "title", "Fortunes", 0);
	
	onion_dict *fortunes=onion_dict_new();
	int i;
	for (i=0;i<fortune_list.count;i++){
		char nr[16];
		snprintf(nr,sizeof(nr),"%010d",nr);
		
		onion_dict *fortune=onion_dict_new();
		onion_dict_add(fortune, "id", fortune_list.list[i].id, 0);
		onion_dict_add(fortune, "message", fortune_list.list[i].message, 0);
		
		onion_dict_add(fortunes, nr, fortune, OD_DUP_KEY|OD_FREE_VALUE|OD_DICT);
	}
	
	onion_dict_add(context,"fortunes",fortunes, OD_DICT|OD_FREE_VALUE);
	
	onion_connection_status ret=fortunes_html_template(context, req, res);
	free(fortune_list.list);
	return ret;
}

#define NCONN 10
// Some data needed by the handler
struct test_data{
	onion_dict *hello;
	MYSQL *db[NCONN];
	int free_db[NCONN];
	pthread_mutex_t mutex;
	sem_t sem;
};

MYSQL *get_connection(struct test_data *data){
	while( 1 ){
		sem_wait(&data->sem);
		pthread_mutex_lock(&data->mutex);
		int i;
		for (i=0;i<NCONN;i++){
			if (data->free_db[i]){
				data->free_db[i]=0;
				pthread_mutex_unlock(&data->mutex);
				return data->db[i];
			}
		}
		
		pthread_mutex_unlock(&data->mutex); // I think it should never get here, but just in case
		sem_post(&data->sem);
	}
}

void free_connection(struct test_data *data, MYSQL *db){
	int i;
	for (i=0;i<NCONN;i++){
		if (data->db[i]==db){
			pthread_mutex_lock(&data->mutex);
			data->free_db[i]=1;
			pthread_mutex_unlock(&data->mutex);
			sem_post(&data->sem);
		}
	}
}

onion_connection_status return_plaintext(onion_request *req, onion_response *res){
	onion_response_set_header(res, "Content-Type","text/plain");
	onion_response_write0(res, "Hello, World!");
	return OCS_PROCESSED;
}

/// Multiplexes to the proper handler depending on the path.
/// As there is no proper database connection pool, take one connection randomly, and uses it.
onion_connection_status muxer(struct test_data *data, onion_request *req, onion_response *res){
	const char *path=onion_request_get_path(req);
	if (strcmp(path, "")==0)
		return return_json(data->hello, req, res);
	if (strcmp(path, "json")==0)
		return return_json_libjson(NULL, req, res);
	
	if (strcmp(path, "db")==0){
		MYSQL *db=get_connection(data);
		int ret=return_db(db, req, res);
		free_connection(data, db);
		return ret;
	}
	if (strcmp(path, "fortune")==0){
		MYSQL *db=get_connection(data);
		int ret=return_fortune(db, req, res);
		free_connection(data, db);
		return ret;
	}
	if (strcmp(path, "plaintext")==0){
		return return_plaintext(req, res);
	}
	
	return OCS_INTERNAL_ERROR;
}

onion *o=NULL;

static void shutdown_server(int _){
	if (o) 
		onion_listen_stop(o);
}


/// Creates the onion http server, creates some server data, creates the handler, listens.
int main(void){
	signal(SIGINT,shutdown_server);
	signal(SIGTERM,shutdown_server);

	o=onion_new(O_POOL);
	
	struct test_data data;
	data.hello=onion_dict_new();
	
	int i;
	for (i=0;i<NCONN;i++){
		data.db[i]=mysql_init(NULL);
		mysql_options(data.db[i], MYSQL_SET_CHARSET_NAME, "utf8");
		data.free_db[i]=1;
		if (data.db[i]==NULL){
			ONION_ERROR("Cant create db connection: %s", mysql_error(data.db[i]));
			return 1;
		}
		if (mysql_real_connect(data.db[i], "localhost", 
                        "benchmarkdbuser", "benchmarkdbpass", "hello_world", 0, NULL, 0) == NULL) {
			ONION_ERROR("Error %u: %s\n", mysql_errno(data.db[i]), mysql_error(data.db[i]));
			return 1;
		}
	}
	pthread_mutex_init(&data.mutex,NULL);
	sem_init(&data.sem,0, NCONN);
	
	onion_dict_add(data.hello,"message","Hello, world", 0);

	onion_set_root_handler(o, onion_handler_new((void*)&muxer, (void*)&data, NULL));

	printf("Listening at http://localhost:8080/\n");
	onion_listen(o);
	
	onion_dict_free(data.hello);
	for (i=0;i<NCONN;i++){
		mysql_close(data.db[i]);
	}
	onion_free(o);
	return 0;
}
