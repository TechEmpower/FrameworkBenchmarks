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
 * stringutils.H
 *
 *  Created on: Apr 9, 2013
 *      Author: xaxaxa
 */

#ifndef URLPARSER_H_
#define URLPARSER_H_
#include <cpoll/cpoll.H>
#include <string>
namespace cppsp
{
#ifndef __CPPSP_TOLOWER
#define __CPPSP_TOLOWER
	static inline char tolower(char c) {
		if (c <= 'Z' && c >= 'A') c = c - 'A' + 'a';
		return c;
	}
#endif
	struct ci_less: std::binary_function<std::string, std::string, bool>
	{
		// case-independent (ci) compare_less binary function
		struct nocase_compare: public std::binary_function<unsigned char, unsigned char, bool>
		{
			bool operator()(const unsigned char& c1, const unsigned char& c2) const {
				return tolower(c1) < tolower(c2);
			}
		};
		bool operator()(const std::string & s1, const std::string & s2) const {
			return std::lexicographical_compare(s1.begin(), s1.end(), // source range
					s2.begin(), s2.end(), // dest range
					nocase_compare()); // comparison
		}
	};

	void urlDecode(const char* in, int inLen, CP::StreamWriter& sw);
	static inline void urlDecode(CP::String in, CP::StreamWriter& sw) {
		urlDecode(in.data(), in.length(), sw);
	}
	CP::String urlDecode(const char* in, int inLen, CP::StringPool& sp);
	void urlEncode(const char* in, int inLen, CP::StreamWriter& sw);
	static inline void urlEncode(CP::String in, CP::StreamWriter& sw) {
		urlEncode(in.data(), in.length(), sw);
	}
	std::string urlDecode(const char* in, int inLen);
	std::string urlEncode(const char* in, int inLen);
	static inline std::string urlDecode(const char* in) {
		return urlDecode(in, strlen(in));
	}
	static inline std::string urlEncode(const char* in) {
		return urlEncode(in, strlen(in));
	}
	static inline std::string urlDecode(std::string in) {
		return urlDecode(in.data(), in.length());
	}
	static inline std::string urlEncode(std::string in) {
		return urlEncode(in.data(), in.length());
	}

	typedef Delegate<void(const char* name, int nameLen, const char* value, int valueLen)> queryStringCallback;
	void parseQueryString(const char* in, int inLen, queryStringCallback cb, bool decode = true);

	void htmlEscape(const char* in, int inLen, CP::StreamWriter& sw);
	static inline void htmlEscape(CP::String in, CP::StreamWriter& sw) {
		htmlEscape(in.data(), in.length(), sw);
	}
	std::string htmlEscape(const char* in, int inLen);
	static inline std::string htmlEscape(const char* in) {
		return htmlEscape(in, strlen(in));
	}
	static inline std::string htmlEscape(std::string in) {
		return htmlEscape(in.data(), in.length());
	}

	void htmlAttributeEscape(const char* in, int inLen, CP::StreamWriter& sw);
	static inline void htmlAttributeEscape(CP::String in, CP::StreamWriter& sw) {
		htmlAttributeEscape(in.data(), in.length(), sw);
	}
	std::string htmlAttributeEscape(const char* in, int inLen);
	static inline std::string htmlAttributeEscape(const char* in) {
		return htmlAttributeEscape(in, strlen(in));
	}
	static inline std::string htmlAttributeEscape(std::string in) {
		return htmlAttributeEscape(in.data(), in.length());
	}

	int ci_compare(CP::String s1, CP::String s2);
	int rfctime(const tm& time, char* c);
}

#endif /* URLPARSER_H_ */
