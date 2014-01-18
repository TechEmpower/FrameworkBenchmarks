/*
 * socketd.H
 *
 *  Created on: Feb 4, 2013
 *      Author: xaxaxa
 */
/*
 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * */
#ifndef SOCKETD_H_
#define SOCKETD_H_
#define SOCKETD_PROT_VERSION 1
#define SOCKETD_MAX_HEADERLEN 1024
namespace socketd
{
	struct protocolHeader
	{
		int version;
		enum
		{
			none = 0, handleConnection, ackConnection, shutdown, attach, attachResponse
		} type;
		protocolHeader() :
				version(SOCKETD_PROT_VERSION), type(none) {
		}
	};
	struct prot_handleConnection
	{
		int64_t id;
		int32_t d;
		int32_t t;
		int32_t p;
		int32_t bufferLen;
	};
	struct prot_ackConnection
	{
		int64_t id;
		bool success;
	};
	struct prot_attach
	{
		int64_t id;
		int namelen;
		int authTokenLen;
		///char name[namelen];
		///char authToken[authTokenLen];
	};
	struct prot_attachResponse
	{
		int64_t id;
		bool success;
	};
}

#endif /* SOCKETD_H_ */
