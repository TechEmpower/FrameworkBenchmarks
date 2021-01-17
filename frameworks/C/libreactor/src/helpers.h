#ifndef HELPERS_H_INCLUDED
#define HELPERS_H_INCLUDED

void plaintext(server_context *context, char *response);

void json(server_context *context, clo *json_object);

void enable_reuseport_cbpf(server *s);

int fork_workers();

#endif /* HELPERS_H_INCLUDED */
