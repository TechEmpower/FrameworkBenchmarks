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
 * split.H
 *
 *  Created on: May 1, 2013
 *      Author: xaxaxa
 */

#ifndef SPLIT_H_
#define SPLIT_H_
#include <cpoll/basictypes.H>
namespace cppsp
{
	struct split
	{
		const char* s;
		const char* end;
		const char* s1;
		CP::String value;
		char delim;
		split(const char* s, int len, char delim) {
			if (len == -1) len = strlen(s);
			this->s = s;
			this->s1 = s;
			this->end = s + len;
			this->delim = delim;
		}
		bool read() {
			if (s == nullptr) return false;
			s = (const char*) memchr(s, delim, end - s);
			if (s == nullptr) {
				value= {s1, int(end - s1)};
				return true;
			}
			value= {s1, int(s - s1)};
			s1 = ++s;
			return true;
		}
	};
}


#endif /* SPLIT_H_ */
