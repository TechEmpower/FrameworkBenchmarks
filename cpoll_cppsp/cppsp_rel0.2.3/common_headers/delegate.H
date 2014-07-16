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
#ifndef DELEGATE_H
#define DELEGATE_H
#include <functional>
#ifndef NULL
#define NULL ((void*)0)
#endif

struct GenericDelegate
{
	void* func;
	void* data;
	template<class R, class... P> R operator()(P && ...p) {
		return ((R (*)(void* data, P...))func)(data,std::forward<P>(p)...);
	}
	
	void* operator=(void* func) {
		return this->func=func;
	}
};

template<class SIGNATURE> struct DelegateBase;
template<class R, class... P>
struct DelegateBase<R(P...)>:public GenericDelegate
{
	/*R (*func)(void* data, P...);
	void* data;*/
	template<class... P2>
	R operator() (P2 && ...p) const {
		return ((R (*)(void* data, P...))func)(data,std::forward<P2>(p)...);
	}
	
	R (*operator=(R (*func)(void* data, P...)))(void* data, P...) {
		this->func=(void*)func;
		return func;
	}
	bool operator==(void* other) {
		return (void*)func == other;
	}
	bool operator==(const DelegateBase<R(P...)>& other) {
		return func == other.func && data==other.data;
	}
	bool operator!=(void* other) {
		return (void*)func != other;
	}
	bool operator!=(const DelegateBase<R(P...)>& other) {
		return func != other.func && data==other.data;
	}
};

template<class SIGNATURE> struct Delegate;
template<class R, class... P>
struct Delegate<R(P...)>:public DelegateBase<R(P...)>
{
	Delegate() {
		this->func=NULL;
	}
	Delegate(R (*func)(void* data, P... p...), void* data): DelegateBase<R(P...)>(func,data) {}
	template<class X>Delegate(R (*func)(X*,P...), X* th) {
		this->func=(void*)(R(*)(void*,P...))func;
		this->data=th;
	}
	template<class X>Delegate(R (X::*func)(P...), X* th) {
		this->func=(void*)(R(*)(void*,P...))func;
		this->data=th;
	}
	Delegate(R (*func)(void*, P...)) {
		this->func=(void*)func;
		this->data=NULL;
	}
	explicit Delegate(const GenericDelegate& other) {
		this->func=other.func;
		this->data=other.data;
	}
	template<class X>Delegate(X* th) {
		this->func=(void*)(R(*)(void*,P...))&X::operator();
		this->data=th;
	}
	Delegate(std::nullptr_t n) {
		this->func=NULL;
	};
	R (*operator=(R (*func)(void* data, P...)))(void* data, P...) {
		this->func=(void*)func;
		return func;
	}
};
template<class R, class... P> static Delegate<R(P...)> nullDelegate() {
	struct {
		R operator()(P... p...) {
			return R();
		}
	} tmp;
	return &tmp;
}

template<class SIGNATURE> class DelegateChain;
template<class R, class ... P>
class DelegateChain<R(P...)>
{
public:
	struct item
	{
		Delegate<R(P...)> func;
		item* prev;
		item* next;
		template<class ... P2>
		inline R operator()(P2 && ...p) const {
			func(std::forward<P2>(p)...);
		}
	};
	item* last = NULL;
	DelegateChain() {

	}
	item* attach(Delegate<R(P...)> func) {
		item* it = new item { func, last, NULL };
		if (last != NULL) last->next = it;
		last = it;
		return it;
	}
	void detach(item* it) {
		if (it->next != NULL) it->next->prev = it->prev;
		if (it->prev != NULL) it->prev->next = it->next;
		if (last == it) last = it->prev;
		delete it;
	}
	inline R operator()(P... p) const {
		if (last != NULL) (*last)(std::forward<P>(p)...);
	}
};
#endif
