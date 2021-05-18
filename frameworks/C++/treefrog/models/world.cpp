#include <TreeFrogModel>
#include "world.h"
#include "worldobject.h"

World::World()
    : TAbstractModel(), d(new WorldObject)
{
    d->id = 0;
    d->randomNumber = 0;
}

World::World(const World &other)
    : TAbstractModel(), d(other.d)
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

int World::randomNumber() const
{
    return d->randomNumber;
}

void World::setRandomNumber(int randomNumber)
{
    d->randomNumber = randomNumber;
}

World &World::operator=(const World &other)
{
    d = other.d;  // increments the reference count of the data
    return *this;
}

bool World::update()
{
    TSqlQueryORMapper<WorldObject> mapper;
    mapper.prepare(QStringLiteral("UPDATE world SET randomNumber=? WHERE id=?"));
    mapper.addBind(randomNumber()).addBind(id());
    return mapper.exec();
}

World World::create(int randomNumber)
{
    WorldObject obj;
    obj.randomNumber = randomNumber;
    if (!obj.create()) {
        return World();
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

World World::get(uint id)
{
    TSqlQueryORMapper<WorldObject> mapper;
    mapper.prepare(QStringLiteral("SELECT * from world WHERE id=?"));
    mapper.addBind(id);
    return World(mapper.execFirst());
}

int World::count()
{
    TSqlORMapper<WorldObject> mapper;
    return mapper.findCount();
}

QList<World> World::getAll()
{
    return tfGetModelListByCriteria<World, WorldObject>(TCriteria());
}

TModelObject *World::modelData()
{
    return d.data();
}

const TModelObject *World::modelData() const
{
    return d.data();
}
