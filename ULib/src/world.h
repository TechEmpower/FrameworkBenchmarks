// world.h

#ifndef WORLD_H
#define WORLD_H 1

#include <ulib/orm/orm.h>
#include <ulib/json/value.h>

class World {
public:
	// Check for memory error
	U_MEMORY_TEST

	// Allocator e Deallocator
	U_MEMORY_ALLOCATOR
	U_MEMORY_DEALLOCATOR

	int id, randomNumber;

	// CONSTRUCTOR

	World()
		{
		U_TRACE_REGISTER_OBJECT(5, World, "")
		}

	World(const World& w)
		{
		U_TRACE_REGISTER_OBJECT(5, World, "%p", &w)

		U_MEMORY_TEST_COPY(w)

		id				 = w.id;
		randomNumber = w.randomNumber;
		}

	~World()
		{
		U_TRACE_UNREGISTER_OBJECT(5, World)
		}

#ifdef DEBUG
	const char* dump(bool breset) const
		{
		*UObjectIO::os << "id           " << id				<< '\n'
							<< "randomNumber " << randomNumber;

		if (breset)
			{
			UObjectIO::output();

			return UObjectIO::buffer_output;
			}

		return 0;
		}
#endif

private:
	World& operator=(const World&) { return *this; }
};

// ORM TEMPLATE SPECIALIZATIONS

template <> class U_EXPORT UOrmTypeHandler<World> : public UOrmTypeHandler_Base {
public:
	explicit UOrmTypeHandler(World& val) : UOrmTypeHandler_Base(&val) {}

	void bindParam(UOrmStatement* stmt) const
		{
		U_TRACE(0, "UOrmTypeHandler<World>::bindParam(%p)", stmt)

		stmt->bindParam(U_ORM_TYPE_HANDLER(World, id,				int));
		stmt->bindParam(U_ORM_TYPE_HANDLER(World, randomNumber,	int));
		}

	void bindResult(UOrmStatement* stmt)
		{
		U_TRACE(0, "UOrmTypeHandler<World>::bindResult(%p)", stmt)

		stmt->bindResult(U_ORM_TYPE_HANDLER(World, id,				int));
		stmt->bindResult(U_ORM_TYPE_HANDLER(World, randomNumber,	int));
		}
};

// JSON TEMPLATE SPECIALIZATIONS

template <> class U_EXPORT UJsonTypeHandler<World> : public UJsonTypeHandler_Base {
public:
	explicit UJsonTypeHandler(World& val) : UJsonTypeHandler_Base(&val) {}

	void toJSON(UValue& json)
		{
		U_TRACE(0, "UJsonTypeHandler<World>::toJSON(%p)", &json)

		json.toJSON(U_JSON_TYPE_HANDLER(World, id,				int));
		json.toJSON(U_JSON_TYPE_HANDLER(World, randomNumber,	int));
		}

	void fromJSON(UValue& json)
		{
		U_TRACE(0, "UJsonTypeHandler<World>::fromJSON(%p)", &json)

		json.fromJSON(U_JSON_TYPE_HANDLER(World, id,				 int));
		json.fromJSON(U_JSON_TYPE_HANDLER(World, randomNumber, int));
		}
};

#endif
