#include <executors/executor.h>

static const char* message = "Hello, World!";

DEFINE_DEFAULT_EXECUTOR(PlainText, STATELESS_EXECUTOR)

DEFINE_EXECUTOR_METHOD(PlainText, GET_METHOD, request, response)
{
	wf_add_http_response_header(response, "Content-Type", "text/plain");

	wf_set_body(response, message, strlen(message));
}

DEFINE_INITIALIZE_WEB_FRAMEWORK()
