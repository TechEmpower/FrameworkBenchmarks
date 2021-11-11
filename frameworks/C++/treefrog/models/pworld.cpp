#include <TreeFrogModel>
#include "pworld.h"
#include "pworldobject.h"

//
// World class for PostgreSQL
//

PWorld::PWorld()
    : TAbstractModel(), d(new PWorldObject)
{
    d->id = 0;
    d->randomnumber = 0;
}

PWorld::PWorld(const PWorld &other)
    : TAbstractModel(), d(other.d)
{ }

PWorld::PWorld(const PWorldObject &object)
    : TAbstractModel(), d(new PWorldObject(object))
{ }

PWorld::~PWorld()
{
    // If the reference count becomes 0,
    // the shared data object 'PWorldObject' is deleted.
}

uint PWorld::id() const
{
    return d->id;
}

int PWorld::randomNumber() const
{
    return d->randomnumber;
}

void PWorld::setRandomNumber(int randomNumber)
{
    d->randomnumber = randomNumber;
}

PWorld &PWorld::operator=(const PWorld &other)
{
    d = other.d;  // increments the reference count of the data
    return *this;
}

bool PWorld::update()
{
    TSqlQueryORMapper<PWorldObject> mapper;
    mapper.prepare(QStringLiteral("UPDATE world SET randomnumber=? WHERE id=?"));
    mapper.addBind(randomNumber()).addBind(id());
    return mapper.exec();
}

PWorld PWorld::create(int randomNumber)
{
    PWorldObject obj;
    obj.randomnumber = randomNumber;
    if (!obj.create()) {
        return PWorld();
    }
    return PWorld(obj);
}

PWorld PWorld::create(const QVariantMap &values)
{
    PWorld model;
    model.setProperties(values);
    if (!model.d->create()) {
        model.d->clear();
    }
    return model;
}

PWorld PWorld::get(uint id)
{
    TSqlQueryORMapper<PWorldObject> mapper;
    mapper.prepare(QStringLiteral("SELECT * from world WHERE id=?"));
    mapper.addBind(id);
    return PWorld(mapper.execFirst());
}

int PWorld::count()
{
    TSqlORMapper<PWorldObject> mapper;
    return mapper.findCount();
}

QList<PWorld> PWorld::getAll()
{
    return tfGetModelListByCriteria<PWorld, PWorldObject>(TCriteria());
}

TModelObject *PWorld::modelData()
{
    return d.data();
}

const TModelObject *PWorld::modelData() const
{
    return d.data();
}
