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

//RGC: simple reference counting based garbage collection framework
#ifndef __RGC_H
#define __RGC_H
#include <cstddef>
#include <utility>
#include <stdlib.h>

namespace RGC
{
	typedef volatile int atomic_t;
#define atomic_read(v)                  v
#define atomic_set(v,i)                 v = i
#define atomic_add(i,v)                 v += i
#define atomic_sub(i,v)                 v -= i
#define atomic_sub_and_test(i,v)        (v -= i) == 0
#define atomic_inc(v)                   v++
#define atomic_dec(v)                   v--
#define atomic_dec_and_test(v)          --v == 0
#define atomic_inc_and_test(v)          ++v == 0
#define atomic_add_negative(i,v)        (v += i) < 0
	//static int objs=0;
	class Allocator;
	class Object
	{
	public:
		Allocator* allocator;
		atomic_t refCount; //reference counter
		Object() {
			allocator = NULL;
			refCount = 1;
			/*objs++;
			 cout << objs << " objects" << endl;*/
		}
		Object(const Object& other) {
			refCount = 1;
		}
		virtual ~Object() {
			/*objs--;
			 cout << objs << " objects" << endl;*/
		}
		inline void retain(int n=1) {
			atomic_add(n,refCount);
		}
		inline void destruct();
		inline bool release(int n=1) { //returns whether or not destruction occurred
			atomic_sub(n,refCount);
			if (refCount <= 0) {
				destruct();
				return true;
			}
			return false;
		}
		/*Object& operator=(const Object& other)
		 {
		 //refCount = 1;
		 return *this;
		 }*/
	};
	class Allocator: public virtual Object
	{
	public:
		virtual void* alloc(int s)=0;
		virtual void dealloc(void* obj)=0;
		template<class T>
		void del(T* obj) {
			obj->~T();
			dealloc(obj);
		}
	};
	class DefaultAllocator: public Allocator
	{
		void* alloc(int s) override final {
			return malloc(s);
		}
		void dealloc(void* obj) override final {
			return free(obj);
		}
	};
	static DefaultAllocator defaultAllocator;
	void Object::destruct() {
		if(allocator==NULL) delete this;
		else allocator->del(this);
	}
	//template<class T> struct Ref;
	template<class T> struct Ref
	{
		T* obj;
		inline Ref() :
				obj(NULL) {
		}
		//inline Ref(const Ref<T>& other);
		inline Ref(T* obj) :
				obj(obj) {
			if (obj != NULL) obj->retain();
		}
		inline Ref(T& obj) :
				obj(&obj) {
			if (&obj != NULL) obj.retain();
		}
		/*template<class ... ARGS> inline Ref(ARGS&& ... args)
		 :obj(new T(std::forward<ARGS>(args)...))
		 {
		 }*/
		inline ~Ref() {
			if (this->obj != NULL) this->obj->release();
		}
		Ref(const Ref<T>& other) :
				obj(other.obj) {
			if (obj != NULL) obj->retain();
		}
		T* operator=(T* obj) {
			if (this->obj != NULL) this->obj->release();
			this->obj = obj;
			if (obj != NULL) obj->retain();
			return obj;
		}
		T* operator=(T& obj) {
			return operator=(*obj);
		}
		Ref& operator=(const Ref& other) {
			if (this->obj != NULL) this->obj->release();
			this->obj = other.obj;
			if (obj != NULL) obj->retain();
			return *this;
		}
		inline T* operator()() const {
			return obj;
		}
		inline T& operator*() const {
			return *obj;
		}
		inline T* operator->() const {
			return obj;
		}
		inline T* get() const {
			return obj;
		}
		template<class A> Ref<A> staticCast() {
			return Ref<A>(static_cast<A*>(obj));
		}
		template<class A> Ref<A> dynamicCast() {
			return Ref<A>(dynamic_cast<A*>(obj));
		}
		template<class A> Ref<A> reinterpretCast() {
			return Ref<A>(reinterpret_cast<A*>(obj));
		}
	};
	//creates a new instance, but disowns it
	template<class T, class ... ARGS> inline T* newObj(ARGS&&... args)
	{
		T* tmp = new T(std::forward<ARGS>(args)...);
		tmp->refCount--;
		return tmp;
	}
/*template<class T> struct Ref
 {
 T* obj;
 template<class ... ARGS> inline Ref(ARGS&& ... args)
 :obj(new T(std::forward<ARGS>(args)...))
 {
 }
 Ref(const Property<T>& prop):obj(prop.obj)
 {
 obj->retain();
 }
 ~Ref()
 {
 obj->release();
 }
 inline T* operator()() const
 {
 return obj;
 }
 inline T* operator->() const
 {
 return obj;
 }
 inline T& operator*() const
 {
 return *obj;
 }
 inline T* get() const
 {
 return obj;
 }
 };*/

//callback mechanism similar to .NET events
			/*template<class funcType> class Event
			 {
			 public:
			 vector<funcType> callbacks;
			 void operator()() {
			 for(int i=0;i<callbacks.size();i++)
			 callbacks[i]();
			 }
			 void operator+=(const funcType& cb) {
			 callbacks.push_back(cb);
			 }

			 };*/
			}
#endif
