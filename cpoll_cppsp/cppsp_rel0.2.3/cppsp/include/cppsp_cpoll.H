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
/*
 * cppsp_socketd.H
 *
 *  Created on: Mar 8, 2013
 *      Author: xaxaxa
 */

#ifndef CPPSP_SOCKETD_H_
#define CPPSP_SOCKETD_H_
#include "page.H"
#include <cpoll/cpoll.H>
#include <string>
#include "httpparser.H"
using namespace std;
typedef CP::String String;
namespace cppsp
{
	class CPollRequest: public Request
	{
	public:
		HTTPParser _parser;
		HTTPStream _stream;
		RGC::Ref<CP::Socket> s;
		Delegate<void(bool)> tmp_cb;
		int _headers_begin;
		bool firstLine;

		CPollRequest(CP::Socket& s, CP::StringPool* sp);
		//returns: true: request already in buffer; false: read is in progress
		bool readRequest(const Delegate<void(bool success)>& cb);
		void readPost(Delegate<void(Request&)> cb) override {
			parsePost(_parser.content);
			cb(*this);
		}
		void _beginRead();
		void _readCB(int i);
		virtual ~CPollRequest();
	};
} /* namespace cppsp */

#endif /* CPPSP_SOCKETD_H_ */
