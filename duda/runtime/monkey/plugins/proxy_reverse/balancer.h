/* Naive
Connects to the first alive server, starting from server.
*/
int proxy_balance_naive(const struct proxy_server_entry_array *server_list,
                        unsigned seed);

/* First-Alive
Connects to the first alive server, starting from 0.
*/
#define proxy_balance_firstalive(server_list) proxy_balance_naive((server_list), 0)

/* Hash
Server is chosen by a hash function of the source IP address. This ensures all requests from a given client will be served by the same server.
*/
int proxy_balance_hash(const struct proxy_server_entry_array *server_list,
                       int sock);

/* Lockless Round Robin
Each consecutive call connects to the next available server. Race conditions can occur since no locking is performed.
*/
int proxy_balance_rr_lockless(const struct proxy_server_entry_array
                              *server_list);

/* Locking Round Robin
Each consecutive call connects to the next available server. Race conditions are prevented at the expense of performance.
*/
int proxy_balance_rr_locking(const struct proxy_server_entry_array
                             *server_list);

/* Least connections
Connects to the server with the least number of connections. Ensures equal load in most use cases but adds significant overhead.
*/
int proxy_balance_leastconnections(const struct proxy_server_entry_array
                                   *server_list, void **connection);

int proxy_balance_init(const struct proxy_entry_array *config);

void proxy_balance_close(void *connection);

/* Statistics
Creates the statistics html if set in the config.
*/

struct string *proxy_balance_generate_statistics(struct session_request *sr);
