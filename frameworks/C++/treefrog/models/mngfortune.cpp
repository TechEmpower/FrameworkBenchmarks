#include <TreeFrogModel>
#include "mngfortune.h"
#include "mngfortuneobject.h"

MngFortune::MngFortune()
    : TAbstractModel(), d(new MngFortuneObject)
{
    d->id = 0;
}

MngFortune::MngFortune(const MngFortune &other)
    : TAbstractModel(), d(other.d)
{ }

MngFortune::MngFortune(const MngFortuneObject &object)
    : TAbstractModel(), d(new MngFortuneObject(object))
{ }

MngFortune::~MngFortune()
{
    // If the reference count becomes 0,
    // the shared data object 'MngFortuneObject' is deleted.
}

int MngFortune::id() const
{
    return d->id;
}

QString MngFortune::message() const
{
    return d->message;
}

void MngFortune::setMessage(const QString &message)
{
    d->message = message;
}

MngFortune &MngFortune::operator=(const MngFortune &other)
{
    d = other.d;  // increments the reference count of the data
    return *this;
}

MngFortune MngFortune::create(const QString &message)
{
    MngFortuneObject obj;
    obj.message = message;
    if (!obj.create()) {
        return MngFortune();
    }
    return MngFortune(obj);
}

MngFortune MngFortune::create(const QVariantMap &values)
{
    MngFortune model;
    model.setProperties(values);
    if (!model.d->create()) {
        model.d->clear();
    }
    return model;
}

MngFortune MngFortune::get(const QString &id)
{
    TMongoODMapper<MngFortuneObject> mapper;
    return MngFortune(mapper.findByObjectId(id));
}

int MngFortune::count()
{
    TMongoODMapper<MngFortuneObject> mapper;
    return mapper.findCount();
}

QList<MngFortune> MngFortune::getAll()
{
    return tfGetModelListByMongoCriteria<MngFortune, MngFortuneObject>(TCriteria());
}

QJsonArray MngFortune::getAllJson()
{
    QJsonArray array;
    TMongoODMapper<MngFortuneObject> mapper;

    if (mapper.find()) {
        while (mapper.next()) {
            array.append(QJsonValue(QJsonObject::fromVariantMap(MngFortune(mapper.value()).toVariantMap())));
        }
    }
    return array;
}

TModelObject *MngFortune::modelData()
{
    return d.data();
}

const TModelObject *MngFortune::modelData() const
{
    return d.data();
}
