// fortune.h

#ifndef FORTUNE_H
#define FORTUNE_H 1

#include <ulib/orm/orm.h>
#include <ulib/json/value.h>

class Fortune {
public:
	// Check for memory error
	U_MEMORY_TEST

	// Allocator e Deallocator
	U_MEMORY_ALLOCATOR
	U_MEMORY_DEALLOCATOR

	UString message;
	int id;

	// CONSTRUCTOR

	Fortune()
		{
		U_TRACE_REGISTER_OBJECT(5, Fortune, "")
		}

	Fortune(int _id, const UString& _message) : message(_message), id(_id)
		{
		U_TRACE_REGISTER_OBJECT(5, Fortune, "%d,%.*S", _id, U_STRING_TO_TRACE(_message))
		}

	Fortune(const Fortune& f)
		{
		U_TRACE_REGISTER_OBJECT(5, Fortune, "%p", &f)

		U_MEMORY_TEST_COPY(f)

		message = f.message;
		id		  = f.id;
		}

	~Fortune()
		{
		U_TRACE_UNREGISTER_OBJECT(5, Fortune)
		}

	// SERVICE

	bool operator<(const Fortune& other) const { return cmp_obj(&message, &other.message); }

	static int cmp_obj(const void* a, const void* b)
		{
		U_TRACE(5, "Fortune::cmp_obj(%p,%p)", a, b)

		return (*(const Fortune**)a)->message.compare((*(const Fortune**)b)->message);
		}

#ifdef DEBUG
	const char* dump(bool breset) const
		{
		*UObjectIO::os << "id               " << id				   << '\n'
							<< "message (UString " << (void*)&message << ')';

		if (breset)
			{
			UObjectIO::output();

			return UObjectIO::buffer_output;
			}

		return 0;
		}
#endif

private:
	Fortune& operator=(const Fortune&) { return *this; }
};

// ORM TEMPLATE SPECIALIZATIONS

template <> class U_EXPORT UOrmTypeHandler<Fortune> : public UOrmTypeHandler_Base {
public:
	explicit UOrmTypeHandler(Fortune& val) : UOrmTypeHandler_Base(&val) {}

	void bindParam(UOrmStatement* stmt) const
		{
		U_TRACE(0, "UOrmTypeHandler<Fortune>::bindParam(%p)", stmt)

		stmt->bindParam(U_ORM_TYPE_HANDLER(Fortune, message, UString));
		stmt->bindParam(U_ORM_TYPE_HANDLER(Fortune, id,		  int));
		}

	void bindResult(UOrmStatement* stmt)
		{
		U_TRACE(0, "UOrmTypeHandler<Fortune>::bindResult(%p)", stmt)

		stmt->bindResult(U_ORM_TYPE_HANDLER(Fortune, message, UString));
		stmt->bindResult(U_ORM_TYPE_HANDLER(Fortune, id,		int));
		}
};

// JSON TEMPLATE SPECIALIZATIONS

template <> class U_EXPORT UJsonTypeHandler<Fortune> : public UJsonTypeHandler_Base {
public:
	explicit UJsonTypeHandler(Fortune& val) : UJsonTypeHandler_Base(&val) {}

	void toJSON(UValue& json)
		{
		U_TRACE(0, "UJsonTypeHandler<Fortune>::toJSON(%p)", &json)

		json.toJSON(U_JSON_TYPE_HANDLER(Fortune, message, UString));
		json.toJSON(U_JSON_TYPE_HANDLER(Fortune, id,		  int));
		}

	void fromJSON(UValue& json)
		{
		U_TRACE(0, "UJsonTypeHandler<Fortune>::fromJSON(%p)", &json)

		json.fromJSON(U_JSON_TYPE_HANDLER(Fortune, message, UString));
		json.fromJSON(U_JSON_TYPE_HANDLER(Fortune, id,		 int));
		}
};

#endif
