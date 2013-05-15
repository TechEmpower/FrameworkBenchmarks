#include <TreeFrogModel>
#include "world.h"
#include "worldobject.h"

World::World()
    : TAbstractModel(), d(new WorldObject)
{
    d->randomnumber = 0;
}

World::World(const World &other)
    : TAbstractModel(), d(new WorldObject(*other.d))
{ }

World::World(const WorldObject &object)
    : TAbstractModel(), d(new WorldObject(object))
{ }

World::~World()
{
    // If the reference count becomes 0,
    // the shared data object 'WorldObject' is deleted.
}

uint World::id() const
{
    return d->id;
}

int World::randomnumber() const
{
    return d->randomnumber;
}

void World::setRandomnumber(int randomnumber)
{
    d->randomnumber = randomnumber;
}

World &World::operator=(const World &other)
{
    d = other.d;  // increments the reference count of the data
    return *this;
}

World World::create(int randomnumber)
{
    WorldObject obj;
    obj.randomnumber = randomnumber;
    if (!obj.create()) {
        obj.clear();
    }
    return World(obj);
}

World World::create(const QVariantMap &values)
{
    World model;
    model.setProperties(values);
    if (!model.d->create()) {
        model.d->clear();
    }
    return model;
}

World World::get(const uint &id)
{
    TSqlORMapper<WorldObject> mapper;
    return World(mapper.findByPrimaryKey(id));
}

QList<World> World::getAll()
{
    return tfGetModelListByCriteria<World, WorldObject>(TCriteria());
}

TSqlObject *World::data()
{
    return d.data();
}

const TSqlObject *World::data() const
{
    return d.data();
}
