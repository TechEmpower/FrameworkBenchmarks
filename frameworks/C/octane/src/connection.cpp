#include <stdlib.h>
#include <octane.h>
#include "connection.hpp"

extern char* current_date;

connection* create_connection()
{
    connection* conn = (connection*)calloc(1, sizeof(connection));
    conn->path = ROUTE_UNKNOWN;
    conn->bytes_remaining = 0;
    conn->request_length = 0;
    conn->keep_alive = true;
    return conn;
}

void free_connection(connection* conn) {
    free(conn);
}
