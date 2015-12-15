/*
 * Connection.h
 *
 *  Created on: 08-May-2014
 *      Author: sumeetc
 */

#ifndef CONNECTION_H_
#define CONNECTION_H_
#include "string"
#include "map"
#include "vector"
using namespace std;

class ConnectionNode {
public:
	float getConnectionTimeout() const;
	void setConnectionTimeout(const float& connectionTimeout);
	const string& getHost() const;
	void setHost(const string& host);
	const string& getPassword() const;
	void setPassword(const string& password);
	int getPort() const;
	void setPort(const int& port);
	float getReadTimeout() const;
	void setReadTimeout(const float& readTimeout);
	const string& getUsername() const;
	void setUsername(const string& username);
	ConnectionNode();
	virtual ~ConnectionNode();
	const string& getDatabaseName() const;
	void setDatabaseName(const string& databaseName);
	const string& getDsn() const;

private:
	string dsn;
	string host;
	string username;
	string password;
	string databaseName;
	int port;
	float readTimeout;
	float connectionTimeout;
	friend class ConnectionPooler;
	friend class ConfigurationHandler;
};

class Connection
{
	bool busy;
	bool type;
	void* _conn;
	ConnectionNode node;
public:
	Connection();
	virtual ~Connection();
	void* getConn();
	void setConn(void* conn);
	bool isBusy() const;
	void setBusy(const bool& busy);
	bool isType() const;
	void setType(const bool& type);
	const ConnectionNode& getNode() const;
	void setNode(const ConnectionNode& node);
};

class ConnectionProperties {
	static string BLANK;
public:
	bool isNewConnectionStrategy() const;
	void setNewConnectionStrategy(const bool& newConnectionStrategy);
	const vector<ConnectionNode>& getNodes() const;
	void setNodes(const vector<ConnectionNode>& nodes);
	void addNode(const ConnectionNode& node);
	int getPoolReadSize() const;
	void setPoolReadSize(const int& poolReadSize);
	int getPoolWriteSize() const;
	void setPoolWriteSize(const int& poolWriteSize);
	const map<string, string>& getProperties() const;
	const string& getProperty(const string& name) const;
	void setProperties(const map<string, string>& properties);
	void addProperty(const string& name, const string& value);
	const string& getType() const;
	void setType(const string& type);
	ConnectionProperties();
	virtual ~ConnectionProperties();
	const string& getName() const;
	void setName(const string& name);

private:
	string name;
	string type;
	int poolReadSize;
	int poolWriteSize;
	bool newConnectionStrategy;
	vector<ConnectionNode> nodes;
	map<string, string> properties;
	friend class ConfigurationHandler;
};

#endif /* CONNECTION_H_ */
