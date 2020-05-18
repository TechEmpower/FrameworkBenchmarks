#ifndef HELPERS_H_INCLUDED
#define HELPERS_H_INCLUDED

void update_date();

void copy_date(char *target, size_t position);

reactor_status custom_reactor_net_bind(reactor_net *net, char *node, char *service);

#endif /* HELPERS_H_INCLUDED */
