/*
 Copyright (c) 2016 Anton Valentinov Kirilov

 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 associated documentation files (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge, publish, distribute,
 sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all copies or
 substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mustache.h>

#include "database.h"
#include "error.h"
#include "fortune.h"
#include "template.h"
#include "utility.h"

typedef struct {
	FILE *input;
	const char *name;
} template_input_t;

static uintmax_t prerender_section(mustache_api_t *api,
                                   void *userdata,
                                   mustache_token_section_t *token);
static uintmax_t prerender_variable(mustache_api_t *api,
                                    void *userdata,
                                    mustache_token_variable_t *token);
static uintmax_t read_template(mustache_api_t *api,
                               void *userdata,
                               char *buffer,
                               uintmax_t buffer_size);
static void template_error(mustache_api_t *api,
                           void *userdata,
                           uintmax_t lineno,
                           const char *error);

static uintmax_t prerender_section(mustache_api_t *api,
                                   void *userdata,
                                   mustache_token_section_t *token)
{
	bool * const in_section = userdata;
	uintmax_t ret = 0;

	if (!*in_section && !strcmp(token->name, FORTUNE_TABLE_NAME)) {
		*in_section = true;
		ret = mustache_prerender(api, userdata, token->section);
		*in_section = false;
	}

	return ret;
}

static uintmax_t prerender_variable(mustache_api_t *api,
                                    void *userdata,
                                    mustache_token_variable_t *token)
{
	IGNORE_FUNCTION_PARAMETER(api);

	bool * const in_section = userdata;
	uintmax_t ret = 0;

	if (*in_section) {
		if (token->text_length == sizeof(ID_FIELD_NAME) - 1 &&
		    !memcmp(token->text, ID_FIELD_NAME, sizeof(ID_FIELD_NAME) - 1)) {
			token->userdata = (void *) offsetof(fortune_t, id);
			ret = 1;
		}
		else if (token->text_length == sizeof(MESSAGE_FIELD_NAME) - 1 &&
		         !memcmp(token->text, MESSAGE_FIELD_NAME, sizeof(MESSAGE_FIELD_NAME) - 1)) {
			token->userdata = (void *) offsetof(fortune_t, message);
			ret = 1;
		}
	}

	return ret;
}

static uintmax_t read_template(mustache_api_t *api,
                               void *userdata,
                               char *buffer,
                               uintmax_t buffer_size)
{
	IGNORE_FUNCTION_PARAMETER(api);

	const template_input_t * const template_input = userdata;

	return fread(buffer, sizeof(*buffer), buffer_size, template_input->input);
}

static void template_error(mustache_api_t *api,
                           void *userdata,
                           uintmax_t lineno,
                           const char *error)
{
	IGNORE_FUNCTION_PARAMETER(api);

	const template_input_t * const template_input = userdata;

	print_error(template_input->name, lineno, "mustache_compile", error);
}

mustache_template_t *get_fortunes_template(const char *path)
{
	mustache_template_t *ret = NULL;
	template_input_t template_input = {.input = fopen(path, "rb"), .name = path};

	if (template_input.input) {
		mustache_api_t api = {.error = template_error,
		                      .read = read_template,
		                      .sectget = prerender_section,
		                      .varget = prerender_variable};
		bool in_section = false;

		ret = mustache_compile(&api, &template_input);

		if (ret && !mustache_prerender(&api, &in_section, ret)) {
			mustache_free(&api, ret);
			ret = NULL;
		}

		fclose(template_input.input);
	}
	else
		LIBRARY_ERROR("fopen");

	return ret;
}
