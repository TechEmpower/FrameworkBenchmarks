/*
Copyright 2010-2012, Strategic Gains, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package hello.controller.persistence;

import java.util.Collection;
import java.util.List;

import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Morphia;
import org.mongodb.morphia.query.Query;
import org.restexpress.common.query.FilterCallback;
import org.restexpress.common.query.FilterComponent;
import org.restexpress.common.query.OrderCallback;
import org.restexpress.common.query.OrderComponent;
import org.restexpress.common.query.QueryFilter;
import org.restexpress.common.query.QueryOrder;
import org.restexpress.common.query.QueryRange;

import com.mongodb.Mongo;
import com.mongodb.MongoClient;
import com.strategicgains.repoexpress.AbstractObservableRepository;
import com.strategicgains.repoexpress.Queryable;
import com.strategicgains.repoexpress.domain.Identifiable;
import com.strategicgains.repoexpress.domain.Identifier;
import com.strategicgains.repoexpress.exception.DuplicateItemException;
import com.strategicgains.repoexpress.exception.InvalidObjectIdException;
import com.strategicgains.repoexpress.exception.ItemNotFoundException;

/**
* Uses MongoDB as its back-end store to persist Identifiable implementations.
* This repository can handle "single-table inheritance" by passing all the
* supported types into the constructor, with the inheritance root listed first.
* 
* @author toddf
* @since Aug 24, 2010
*/
public class MongodbRepository<T extends Identifiable>
extends AbstractObservableRepository<T>
implements Queryable<T>
{
private MongoClient mongo;
private Morphia morphia;
private Datastore datastore;
private Class<T> inheritanceRoot;

/**
 * 
 * @param mongo a pre-configured Mongo instance.
 * @param dbName the name of the database (in MongoDB).
 * @param entityClasses Class(es) managed by this repository. Inheritance root first.
 */
public MongodbRepository(MongoClient mongo, String dbName, boolean skipEnsureSteps, Class<? extends T>... entityClasses)
{
	super();
	this.mongo = mongo;
	initialize(dbName, skipEnsureSteps, entityClasses);
}

@SuppressWarnings("unchecked")
private void initialize(String name, boolean skipEnsureSteps, Class<? extends T>... entityClasses)
{
	morphia = new Morphia();
	inheritanceRoot = (Class<T>) entityClasses[0];

	for (Class<?> entityClass : entityClasses)
	{
		morphia.map(entityClass);
	}

	datastore = morphia.createDatastore(mongo, name);
	if (!skipEnsureSteps) {
		datastore.ensureIndexes();
		datastore.ensureCaps();
	}
}

@Override
public T doCreate(T item)
{
	if (exists(item.getId()))
	{
		throw new DuplicateItemException(item.getClass().getSimpleName()
		    + " ID already exists: " + item.getId());
	}

	datastore.save(item);
	return item;
}

@Override
public T doRead(Identifier id)
{
	T item = datastore.get(inheritanceRoot, id.primaryKey());

	if (item == null)
	{
		throw new ItemNotFoundException("ID not found: " + id);
	}

	return item;
}

@Override
public T doUpdate(T item)
{
	if (!exists(item.getId()))
	{
		throw new ItemNotFoundException(item.getClass().getSimpleName()
		    + " ID not found: " + item.getId());
	}

	datastore.save(item);
	return item;
}

@Override
public void doDelete(T object)
{
	try
	{
		datastore.delete(object);
	}
	catch (InvalidObjectIdException e)
	{
		throw new ItemNotFoundException("ID not found: " + object.getId());
	}
}

/**
 * A general-purpose 'finder' method, useful for implementing alternate-key queries. Since
 * it does not support ordering and range sub-sets, it's best for creating queries that
 * return a list size of 1.
 * <p/>
 * Essentially, just calls readAll() with null range and order.  So if you need ordering,
 * call readAll(filter, null, order).
 * 
 * @param filter query criteria.
 * @return
 */
public List<T> find(QueryFilter filter)
{
	return readAll(filter, null, null);
}

/**
 * Implements a 'default' readAll' method that queries for all instances of the inheritance
 * root class matching the given criteria.
 * 
 * This method does not invoke an observer method, so is not observable by default.  Override,
 * calling super() to get that functionality, or call notifyBeforeXXX() and/or notifyAfterXXX()
 * methods, if desired.
 * 
 * @param filter
 * @param range
 * @param order
 * @return a list of results. Never null.
 */
@Override
public List<T> readAll(QueryFilter filter, QueryRange range, QueryOrder order)
{
	return query(inheritanceRoot, filter, range, order);
}

/**
 * Read each of the instances corresponding to the given Collection of IDs, returning the 
 * results as a list.  If an ID in the provided Collection does not exist, it is simply
 * not included in the returned results.
 * 
 * @param ids a Collection of IDs to read.
 */
@Override
public List<T> readList(Collection<Identifier> ids)
{
	return getDataStore().find(inheritanceRoot).field("_id").in(new PrimaryIdIterable(ids)).asList();
}

/**
 * Count the instances of the inheritance root (class) that match the given filter criteria.
 * 
 * @param filter
 */
@Override
public long count(QueryFilter filter)
{
	return count(inheritanceRoot, filter);
}

/**
 * Count the instances of the given type matching the given filter criteria.
 * 
 * @param type
 * @param filter
 */
public long count(Class<T> type, QueryFilter filter)
{
	return getBaseFilterQuery(type, filter).countAll();
}

/**
 * Returns true if the given id exists in the repository.
 * 
 * @param id the identifier of the object.
 */
@Override
public boolean exists(Identifier id)
{
	if (id == null) return false;

	return (datastore.getCount(datastore.find(inheritanceRoot, "_id", id.primaryKey())) > 0);

	// is the above line more efficient, or the following one?
//	return (datastore.find(inheritanceRoot, "_id", adaptId(id)).countAll() > 0);
}


// SECTION: UTILITY

/**
 * Get the underlying Morphia Datastore object with which to construct queries against.
 * 
 * @return the underlying Morphia Datastore.
 */
protected Datastore getDataStore()
{
	return datastore;
}

/**
 * Return the underlying Mongo instance.
 * 
 * @return the underlying Mongo instance.
 */
protected Mongo getMongo()
{
	return mongo;
}

/**
 * Execute a query against the repository, using QueryFilter, QueryRange and QueryOrder
 * as criteria against the type.  Returns the results as a List.
 * 
 * @param type
 * @param range
 * @param filter
 * @param order
 */
protected List<T> query(Class<T> type, QueryFilter filter, QueryRange range, QueryOrder order)
{
	return getBaseQuery(type, filter, range, order).asList();
}

/**
 * Create and configure a basic query utilizing provided QueryFilter, QueryRange and QueryOrder
 * criteria, returning the query.
 * 
 * @param type
 * @param range
 * @param filter
 * @param order
 */
protected Query<T> getBaseQuery(Class<T> type, QueryFilter filter, QueryRange range, QueryOrder order)
{
	Query<T> q = getBaseFilterQuery(type, filter);
	configureQueryRange(q, range);
	configureQueryOrder(q, order);
	return q;
}

/**
 * Create and configure a basic query utilizing just QueryFilter as criteria.
 * 
 * @param type
 * @param filter
 * @return a Morphia Query instance configured for the QueryFilter criteria.
 */
private Query<T> getBaseFilterQuery(Class<T> type, QueryFilter filter)
{
	Query<T> q = getDataStore().find(type);
	configureQueryFilter(q, filter);
	return q;
}

/**
 * @param q
 * @param range
 */
private void configureQueryRange(Query<T> q, QueryRange range)
{
	if (range == null) return;

	if (range.isInitialized())
	{
		q.offset((int) range.getStart());
		q.limit(range.getLimit());
	}
}

private void configureQueryFilter(final Query<T> q, QueryFilter filter)
{
	if (filter == null) return;

	filter.iterate(new FilterCallback()
	{
		@Override
		public void filterOn(FilterComponent c)
		{
			switch(c.getOperator())
			{
				case CONTAINS:		// String-related
					q.field(c.getField()).contains((c.getValue().toString()));
					break;
				case STARTS_WITH:	// String-related
					q.field(c.getField()).startsWith(c.getValue().toString());
					break;
				case GREATER_THAN:
					q.field(c.getField()).greaterThan(c.getValue());
					break;
				case GREATER_THAN_OR_EQUAL_TO:
					q.field(c.getField()).greaterThanOrEq(c.getValue());
					break;
				case LESS_THAN:
					q.field(c.getField()).lessThan(c.getValue());
					break;
				case LESS_THAN_OR_EQUAL_TO:
					q.field(c.getField()).lessThanOrEq(c.getValue());
					break;
				case NOT_EQUALS:
					q.field(c.getField()).notEqual(c.getValue());
					break;
				case IN:
					q.field(c.getField()).in((Iterable<?>) c.getValue());
					break;
				case EQUALS:
				default:
					q.field(c.getField()).equal(c.getValue());
					break;
			}
		}
	});
}

/**
 * @param q
 * @param order
 */
private void configureQueryOrder(Query<T> q, QueryOrder order)
{
	if (order == null) return;

	if (order.isSorted())
	{
		final StringBuilder sb = new StringBuilder();
		
		order.iterate(new OrderCallback()
		{
			boolean isFirst = true;

			@Override
			public void orderBy(OrderComponent component)
			{
				if (!isFirst)
				{
					sb.append(',');
				}
				
				if (component.isDescending())
				{
					sb.append('-');
				}

				sb.append(component.getFieldName());
				isFirst = false;
			}
		});
		
		q.order(sb.toString());
	}
}
}