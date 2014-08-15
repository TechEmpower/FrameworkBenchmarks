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
 * headerContainer.H
 *
 *  Created on: Apr 28, 2013
 *      Author: xaxaxa
 */

#ifndef HEADERCONTAINER_H_
#define HEADERCONTAINER_H_
#include <ctype.h>
#include <sys/types.h>
#include <algorithm>
#include "stringutils.H"
namespace cppsp
{
	typedef CP::String String;
#ifndef __CPPSP_TOLOWER
#define __CPPSP_TOLOWER
	static inline char tolower(char c) {
		if (c <= 'Z' && c >= 'A') c = c - 'A' + 'a';
		return c;
	}
#endif
	struct Header
	{
		String name;
		String value;
	};
	//sorted
	struct headerContainer
	{
		RGC::Allocator* a;
		struct item
		{
			const char* name;
			const char* value;
			int nameLength;
			int valueLength;
			String n() {
				return {name,nameLength};
			}
			String v() {
				return {value,valueLength};
			}
		};
		static bool compareItem(const item& i1, const item& i2) {
			return ci_compare( { i1.name, i1.nameLength }, { i2.name, i2.nameLength }) < 0;
		}
		struct iterator
		{
			headerContainer* c;
			item* i;
			void operator+=(int i) {
				this->i += i;
				if (this->i >= (c->items + c->length)) this->i = NULL;
			}
			void operator++(int) {
				operator+=(1);
			}
			bool operator==(const iterator& other) {
				return i == other.i;
			}
			bool operator!=(const iterator& other) {
				return !operator==(other);
			}
			Header operator*() {
				item& it = *i;
				return { {it.name,it.nameLength}, {it.value,it.valueLength}};
			}
			item& get() {
				return *i;
			}
		};
		item* items;
		int length;
		headerContainer(RGC::Allocator* a) :
				a(a), items(NULL), length(0) {
		}
		item* beginReplace(int length) {
			items = (item*) a->alloc(length * sizeof(item));
			this->length = length;
			return items;
		}
		void endReplace() {
			std::sort(items, items + length, compareItem);
		}
		String operator[](String name) const {
			item it { name.data(), NULL, name.length(), 0 };
			item* tmp = std::lower_bound(items, items + length, it, compareItem);
			if (tmp != NULL && (tmp - items) < length && ci_compare(tmp->n(), name) == 0) return tmp->v();
			return {(char*)nullptr,0};
		}
		iterator find(String name) {
			item it { name.data(), NULL, name.length(), 0 };
			item* tmp = std::lower_bound(items, items + length, it, compareItem);
			if (tmp != NULL && (tmp - items) < length && ci_compare(tmp->n(), name) == 0) return {this,tmp};
			return end();
		}
		iterator begin() {
			return {this,items};
		}
		iterator end() {
			return {this,NULL};
		}
		void clear() {
			items = NULL;
			length = 0;
		}

	};
	//not sorted; for response headers
	struct headerContainer2
	{
		CP::StringPool* sp;
		struct item
		{
			const char* name;
			const char* value;
			int nameLength;
			int valueLength;
		};
		static const int bucketSize = 8;
		struct bucket
		{
			bucket* next;
			item items[bucketSize];
			int length;
		};
		struct iterator
		{
			bucket* b;
			int i;
			void operator+=(int i) {
				this->i += i;
				while (this->i > bucketSize && b != NULL) {
					b = b->next;
					this->i -= bucketSize;
				}
				if (b != NULL && this->i >= b->length) b = NULL;
			}
			void operator++(int) {
				operator+=(1);
			}
			bool operator==(const iterator& other) {
				if (b == NULL && other.b == NULL) return true;
				return b == other.b && i == other.i;
			}
			bool operator!=(const iterator& other) {
				return !operator==(other);
			}
			Header operator*() {
				item& it = b->items[i];
				return { {it.name,it.nameLength}, {it.value,it.valueLength}};
			}
			item& get() {
				return b->items[i];
			}
		};
		bucket* _first = NULL;
		bucket* _last = NULL;
		headerContainer2(CP::StringPool* sp) :
				sp(sp) {
		}
		void add(item it) {
			if (_last == NULL || _last->length >= bucketSize) addBucket();
			_last->items[_last->length] = it;
			_last->length++;
		}
		void add(String name, String value) {
			add( { name.data(), value.data(), name.length(), value.length() });
		}
		void addCopy(String name, String value) {
			name = sp->addString(name);
			value = sp->addString(value);
			add( { name.data(), value.data(), name.length(), value.length() });
		}
		void addBucket() {
			bucket* b = (bucket*) sp->add(sizeof(bucket));
			b->next = NULL;
			b->length = 0;
			if (_last != NULL) _last->next = b;
			_last = b;
			if (_first == NULL) _first = b;
		}
		String operator[](String name) {
			for (bucket* b = _first; b != NULL; b = b->next) {
				for (int i = 0; i < b->length; i++)
					if (ci_compare(name, { b->items[i].name, b->items[i].nameLength }) == 0) return {b->items[i].value,b->items[i].valueLength};
			}
			return {(char*)nullptr,0};
		}
		iterator find(String name) {
			for (bucket* b = _first; b != NULL; b = b->next) {
				for (int i = 0; i < b->length; i++)
					if (ci_compare(name, { b->items[i].name, b->items[i].nameLength }) == 0) return {b,i};
			}
			return end();
		}
		void set(String name, String value) {
			iterator it = find(name);
			if (it == end()) add(name, value);
			else {
				it.get().value = value.data();
				it.get().valueLength = value.length();
			}
		}
		iterator begin() {
			return {_first,0};
		}
		iterator end() {
			return {NULL,0};
		}
		void clear() {
			_first = _last = NULL;
		}
	};
}

#endif /* HEADERCONTAINER_H_ */
