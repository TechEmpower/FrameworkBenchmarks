#include <TreeFrogModel>
#include "fortune.h"
#include "fortuneobject.h"

Fortune::Fortune()
    : TAbstractModel(), d(new FortuneObject)
{
    d->id = 0;
}

Fortune::Fortune(const Fortune &other)
    : TAbstractModel(), d(other.d)
{ }

Fortune::Fortune(const FortuneObject &object)
    : TAbstractModel(), d(new FortuneObject(object))
{ }

Fortune::~Fortune()
{
    // If the reference count becomes 0,
    // the shared data object 'FortuneObject' is deleted.
}

uint Fortune::id() const
{
    return d->id;
}

QString Fortune::message() const
{
    return d->message;
}

void Fortune::setMessage(const QString &message)
{
    d->message = message;
}

Fortune &Fortune::operator=(const Fortune &other)
{
    d = other.d;  // increments the reference count of the data
    return *this;
}

Fortune Fortune::create(const QString &message)
{
    FortuneObject obj;
    obj.message = message;
    if (!obj.create()) {
        return Fortune();
    }
    return Fortune(obj);
}

Fortune Fortune::create(const QVariantMap &values)
{
    Fortune model;
    model.setProperties(values);
    if (!model.d->create()) {
        model.d->clear();
    }
    return model;
}

Fortune Fortune::get(uint id)
{
    TSqlORMapper<FortuneObject> mapper;
    return Fortune(mapper.findByPrimaryKey(id));
}

int Fortune::count()
{
    TSqlORMapper<FortuneObject> mapper;
    return mapper.findCount();
}

QList<Fortune> Fortune::getAll()
{
    TSqlQueryORMapper<FortuneObject> mapper;
    mapper.prepare(QStringLiteral("SELECT * from fortune"));

    QList<Fortune> fortunes;
    if (mapper.exec()) {
        while (mapper.next()) {
            fortunes << Fortune(mapper.value());
        }
    }
    return fortunes;
}

TModelObject *Fortune::modelData()
{
    return d.data();
}

const TModelObject *Fortune::modelData() const
{
    return d.data();
}
