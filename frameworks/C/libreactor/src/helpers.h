#ifndef HELPERS_H_INCLUDED
#define HELPERS_H_INCLUDED

reactor_vector http_date_header(int update);

reactor_vector http_content_length_header(uint32_t n);

void write_response(reactor_stream *stream, reactor_vector preamble, reactor_vector body);

reactor_status custom_reactor_net_bind(reactor_net *net, char *node, char *service);

#endif /* HELPERS_H_INCLUDED */
