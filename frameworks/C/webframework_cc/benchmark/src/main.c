#include <stdio.h>

#include <import.h>

static void on_start();

int main(int argc, char** argv)
{
	wf_initialize_web_framework("WebFramework");

	web_framework_t server = NULL;
	web_framework_exception_t exception = wf_create_web_framework_from_path("config.json", &server);

	if (exception)
	{
		fprintf(stderr, "%s\n", wf_get_error_message(exception));

		wf_delete_web_framework_exception(exception);

		return 1;
	}

	exception =  wf_start_web_framework_server(server, true, on_start);

	if (exception)
	{
		fprintf(stderr, "%s\n", wf_get_error_message(exception));

		wf_delete_web_framework(server);

		wf_delete_web_framework_exception(exception);

		return 2;
	}

	return 0;
}

void on_start()
{
	printf("Server is running...\n");
}
