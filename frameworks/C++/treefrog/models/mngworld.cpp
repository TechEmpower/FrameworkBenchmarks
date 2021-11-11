#include <TreeFrogModel>
#include "mngworld.h"
#include "mngworldobject.h"

MngWorld::MngWorld()
    : TAbstractModel(), d(new MngWorldObject)
{
    d->id = 0;
    d->randomNumber = 0;
}

MngWorld::MngWorld(const MngWorld &other)
    : TAbstractModel(), d(other.d)
{ }

MngWorld::MngWorld(const MngWorldObject &object)
    : TAbstractModel(), d(new MngWorldObject(object))
{ }

MngWorld::~MngWorld()
{
    // If the reference count becomes 0,
    // the shared data object 'MngWorldObject' is deleted.
}

int MngWorld::id() const
{
    return d->id;
}

int MngWorld::randomNumber() const
{
    return d->randomNumber;
}

void MngWorld::setRandomNumber(int randomNumber)
{
    d->randomNumber = randomNumber;
}

MngWorld &MngWorld::operator=(const MngWorld &other)
{
    d = other.d;  // increments the reference count of the data
    return *this;
}

MngWorld MngWorld::create(int randomNumber)
{
    MngWorldObject obj;
    obj.randomNumber = randomNumber;
    if (!obj.create()) {
        return MngWorld();
    }
    return MngWorld(obj);
}

MngWorld MngWorld::create(const QVariantMap &values)
{
    MngWorld model;
    model.setProperties(values);
    if (!model.d->create()) {
        model.d->clear();
    }
    return model;
}

MngWorld MngWorld::get(const QString &id)
{
    TMongoODMapper<MngWorldObject> mapper;
    return MngWorld(mapper.findByObjectId(id));
}

int MngWorld::count()
{
    TMongoODMapper<MngWorldObject> mapper;
    return mapper.findCount();
}

QList<MngWorld> MngWorld::getAll()
{
    return tfGetModelListByMongoCriteria<MngWorld, MngWorldObject>(TCriteria());
}

QJsonArray MngWorld::getAllJson()
{
    QJsonArray array;
    TMongoODMapper<MngWorldObject> mapper;

    if (mapper.find()) {
        while (mapper.next()) {
            array.append(QJsonValue(QJsonObject::fromVariantMap(MngWorld(mapper.value()).toVariantMap())));
        }
    }
    return array;
}

TModelObject *MngWorld::modelData()
{
    return d.data();
}

const TModelObject *MngWorld::modelData() const
{
    return d.data();
}
