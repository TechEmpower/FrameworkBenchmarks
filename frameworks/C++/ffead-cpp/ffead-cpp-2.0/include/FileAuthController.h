/*
 * FileAuthController.h
 *
 *  Created on: Nov 23, 2010
 *      Author: sumeet
 */

#ifndef FILEAUTHCONTROLLER_H_
#define FILEAUTHCONTROLLER_H_
#include "AuthController.h"
#include "fstream"

class FileAuthController :public AuthController
{
	string filename;
	string delimiter;
public:
	string treat_password(const string&);
	FileAuthController(const string&, const string&);
	virtual ~FileAuthController();
	bool authenticate(const string&, const string&);
	bool isInitialized();
	bool getPassword(const string& username, string &passwd);
	string getUserRole(const string&);
	string get(const string& username, const int& pos);
};

#endif /* FILEAUTHCONTROLLER_H_ */
