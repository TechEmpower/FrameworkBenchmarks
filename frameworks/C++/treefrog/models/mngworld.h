#ifndef MNGWORLD_H
#define MNGWORLD_H

#include <QStringList>
#include <QDateTime>
#include <QVariant>
#include <QSharedDataPointer>
#include <TGlobal>
#include <TAbstractModel>

class TModelObject;
class MngWorldObject;
class QJsonArray;


class T_MODEL_EXPORT MngWorld : public TAbstractModel
{
public:
    MngWorld();
    MngWorld(const MngWorld &other);
    MngWorld(const MngWorldObject &object);
    ~MngWorld();

    int id() const;
    int randomNumber() const;
    void setRandomNumber(int randomNumber);
    MngWorld &operator=(const MngWorld &other);

    bool create() { return TAbstractModel::create(); }
    bool update() { return TAbstractModel::update(); }
    bool save()   { return TAbstractModel::save(); }
    bool remove() { return TAbstractModel::remove(); }

    static MngWorld create(int randomNumber);
    static MngWorld create(const QVariantMap &values);
    static MngWorld get(const QString &id);
    static int count();
    static QList<MngWorld> getAll();
    static QJsonArray getAllJson();

private:
    QSharedDataPointer<MngWorldObject> d;

    TModelObject *modelData();
    const TModelObject *modelData() const;
};

Q_DECLARE_METATYPE(MngWorld)
Q_DECLARE_METATYPE(QList<MngWorld>)

#endif // MNGWORLD_H
