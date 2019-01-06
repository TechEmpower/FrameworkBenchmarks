#ifndef MNGWORLDOBJECT_H
#define MNGWORLDOBJECT_H

#include <TMongoObject>
#include <QSharedData>


class T_MODEL_EXPORT MngWorldObject : public TMongoObject, public QSharedData
{
public:
    QString _id;
    int id;
    int randomNumber;

    enum PropertyIndex {
        _Id = 0,
        Id,
        RandomNumber,
    };

    virtual QString collectionName() const { return QStringLiteral("world"); }
    virtual QString objectId() const { return _id; }
    virtual QString &objectId() { return _id; }

private:
    Q_OBJECT
    Q_PROPERTY(QString _id READ get_id WRITE set_id)
    T_DEFINE_PROPERTY(QString, _id)
    Q_PROPERTY(int id READ getid WRITE setid)
    T_DEFINE_PROPERTY(int, id)
    Q_PROPERTY(int randomNumber READ getrandomNumber WRITE setrandomNumber)
    T_DEFINE_PROPERTY(int, randomNumber)
};

#endif // MNGWORLDOBJECT_H
