template<class T, int max>class genericPool
{
public:
	Delegate<T()> doCreate;
	Delegate<void(T)> doDestroy;
	genericPool(Delegate<T()> c, Delegate<void(T)> d):doCreate(c),doDestroy(d) {}
	struct item {
		T data;
	};
	item items[max];
	int itemCount=0;
	T get() {
		if(itemCount>0) {
			return items[--itemCount].data;
		}
		return doCreate();
	}
	void put(T obj) {
		if(itemCount>=max) doDestroy(obj);
		else {
			items[itemCount++].data=obj;
		}
	}
	~genericPool() {
		for(int i=0;i<itemCount;i++) {
			doDestroy(items[i].data);
		}
	}
};
