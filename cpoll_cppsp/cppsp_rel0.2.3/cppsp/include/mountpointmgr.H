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
 * mountpointmgr.H
 *
 *  Created on: May 1, 2013
 *      Author: xaxaxa
 */

#ifndef MOUNTPOINTMGR_H_
#define MOUNTPOINTMGR_H_
#include <map>
#include <cpoll/cpoll.H>
#include <rgc.H>
#include "split.H"
using namespace CP;
using namespace RGC;
using namespace std;
namespace cppsp
{
	template<class T> struct less;
	template<>
	struct less<String> : binary_function<String, String, bool>
	{
		bool operator()(const String& x, const String& y) const {
			if (x.len == 0 || y.len == 0) return x.len < y.len;
			int complen = x.len < y.len ? x.len : y.len;
			int tmp = memcmp(x.d, y.d, complen);
			if (tmp == 0) return x.len < y.len;
			else return tmp < 0;
		}
	};
#if false
	class MountPointMgr
	{
	public:
		map<String, Object*, cppsp::less<String> > items;

		Object* _find(String path) {
			auto it = items.upper_bound(path);
			if (it == items.end() && items.begin() == items.end()) return nullptr;
			it--;
			if (it == items.end()) return nullptr;
			if ((*it).first.len > path.len) return nullptr;
			int complen = (*it).first.len < path.len ? (*it).first.len : path.len;
			if (memcmp((*it).first.d, path.d, complen) == 0) return (*it).second;
			return nullptr;
		}
		Object* find(String path) {
			if (path.len > 0 && path.d[path.len - 1] == '/') {
				return _find(path);
			} else {
				char tmp[path.len + 1];
				memcpy(tmp, path.d, path.len);
				tmp[path.len] = '/';
				return _find( {tmp, path.len + 1});
			}
		}
		bool mount(String path, Object* handler) {
			String tmp;
			if (path.len > 0 && path.d[path.len - 1] == '/') {
				tmp.len = path.len;
				tmp.d = (char*) malloc(tmp.len + 1); //insert null byte just in case
				memcpy(tmp.d, path.d, tmp.len);
				tmp.d[tmp.len] = '\0';
			} else {
				tmp.len = path.len + 1;
				tmp.d = (char*) malloc(tmp.len + 1);
				memcpy(tmp.d, path.d, path.len);
				tmp.d[path.len] = '/';
				tmp.d[tmp.len] = '\0';
			}
			auto it = items.find(tmp);
			if (it == items.end()) {
				items.insert( {tmp, handler});
				return true;
			} else {
				free(tmp.d);
				return false;
			}
		}
		Object* umount(String path) {
			String tmp;
			if (path.len > 0 && path.d[path.len - 1] == '/') {
				tmp.d = nullptr;
			} else {
				tmp.len = path.len + 1;
				tmp.d = (char*) malloc(tmp.len + 1);
				memcpy(tmp.d, path.d, path.len);
				tmp.d[path.len] = '/';
				tmp.d[tmp.len] = '\0';
			}
			auto it = items.find(tmp.d == nullptr ? path : tmp);
			Object* o;
			if (it != items.end()) {
				o = (*it).second;
				free((*it).first.d);
				items.erase(it);
			} else {
				o = nullptr;
			}
			if (tmp.d != nullptr) free(tmp.d);
			return o;
		}
		MountPointMgr() {

		}
		virtual ~MountPointMgr() {

		}
	};
#endif
	class MountPointMgr
	{
	public:
		struct dirent
		{
			Object* obj;
			void* children;
			dirent() :
					obj(nullptr), children(nullptr) {
			}
		};
		typedef map<String, dirent, cppsp::less<String> > Map;
		dirent root;

		Object* find(String path) {
			split spl(path.d, path.len, '/');
			dirent* curr = &root;
			Object* o = root.obj;
			while (spl.read()) {
				if (spl.value.len <= 0) goto aaaaa;
				{
					//printf("spl.value=(len=%i) %s\n", spl.value.len, spl.value.toSTDString().c_str());
					if (curr->children == nullptr) {
						//printf("curr->children==NULL\n");
						break;
					}
					Map* m = (Map*) curr->children;
					auto it = m->find(spl.value);
					if (it == m->end()) {
						//printf("it == m->end(); spl.value=%s\n", spl.value.toSTDString().c_str());
						break;
					}
					curr = &(*it).second;
					if (curr->obj != NULL) o = curr->obj;
				}
				aaaaa: ;
			}
			return o;
		}
		bool mount(String path, Object* handler) {
			split spl(path.d, path.len, '/');
			dirent* curr = &root;
			while (spl.read()) {
				if (spl.value.len <= 0) goto aaaaa;
				{
					//printf("spl.value=(len=%i) %s\n", spl.value.len, spl.value.toSTDString().c_str());
					Map* m;
					if (curr->children == nullptr) {
						curr->children = m = new Map();
					} else {
						m = (Map*) curr->children;
					}
					auto it = m->find(spl.value);
					if (it == m->end()) {
						String s;
						s.d = (char*) malloc((s.len = spl.value.len));
						memcpy(s.d, spl.value.d, s.len);
						curr = &(*m)[s];
					} else curr = &(*it).second;
				}
				aaaaa: ;
			}
			if (curr->obj == nullptr) {
				curr->obj = handler;
				return true;
			}
			return false;
		}
		Object* umount(String path) {
			split spl(path.d, path.len, '/');
			dirent* curr = &root;
			while (spl.read()) {
				if (spl.value.len <= 0) goto aaaaa;
				{
					Map* m;
					if (curr->children == nullptr) {
						return nullptr;
					} else {
						m = (Map*) curr->children;
					}
					auto it = m->find(spl.value);
					if (it == m->end()) {
						return nullptr;
					} else curr = &(*it).second;
				}
				aaaaa: ;
			}
			Object* o = curr->obj;
			curr->obj = NULL;
			return o;
		}
		template<class SW>
		void print(SW& sw, dirent* d, string s = "/", int l = 0) {
			char tmp[l + 1];
			memset(tmp, ' ', l);
			tmp[l] = 0;
			if (d->obj != NULL) sw.writeF("%s%s\n", (char*) tmp, s.c_str());
			if (d->children == nullptr) {
				return;
			} else {
				Map* m = (Map*) d->children;
				for (auto it = m->begin(); it != m->end(); it++) {
					print(sw, &(*it).second, s + (*it).first.toSTDString() + "/", l + 1);
				}
			}
		}
		template<class SW>
		void print(SW& sw) {
			print(sw, &root);
		}
		void clear(dirent* d) {
			if (d->obj != nullptr) {
				d->obj->destruct();
				d->obj = nullptr;
			}
			if (d->children != nullptr) {
				Map* m = (Map*) d->children;
				for (auto it = m->begin(); it != m->end(); it++) {
					clear(&(*it).second);
					String s = (*it).first;
					free(s.d);
				}
				delete m;
				d->children = nullptr;
			}
		}
		void clear() {
			clear(&root);
		}
		MountPointMgr() {

		}
		virtual ~MountPointMgr() {
			clear();
		}
	};
} /* namespace cppsp */
#endif /* MOUNTPOINTMGR_H_ */
