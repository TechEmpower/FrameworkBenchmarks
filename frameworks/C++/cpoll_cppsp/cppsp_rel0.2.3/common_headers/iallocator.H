#include <rgc.H>
class IAllocator: virtual public RGC::Object
{
	//returns NULL if failed
	virtual void* allocate(int32_t len);
	virtual void free(void* obj);
	//if failed, returns NULL and leaves original object untouched
	virtual void* reallocate(void* obj, int32_t len);
	virtual ~IAllocator();
};

