#include <executors/executor.h>

DEFINE_DEFAULT_EXECUTOR(Json, STATELESS_EXECUTOR)

DEFINE_EXECUTOR_METHOD(Json, GET_METHOD, request, response)
{
	json_builder_t result = NULL;

	wf_create_json_builder(&result);

	wf_append_json_builder_string(result, "message", "Hello, World!");

	wf_set_json_body(response, result);

	wf_delete_json_builder(result);
}
