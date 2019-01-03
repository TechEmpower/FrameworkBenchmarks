#ifndef MNGFORTUNEOBJECT_H
#define MNGFORTUNEOBJECT_H

#include <TMongoObject>
#include <QSharedData>


class T_MODEL_EXPORT MngFortuneObject : public TMongoObject, public QSharedData
{
public:
    QString _id;
    int id;
    QString message;

    enum PropertyIndex {
        _Id = 0,
        Id,
        Message,
    };

    virtual QString collectionName() const { return QStringLiteral("fortune"); }
    virtual QString objectId() const { return _id; }
    virtual QString &objectId() { return _id; }

private:
    Q_OBJECT
    Q_PROPERTY(QString _id READ get_id WRITE set_id)
    T_DEFINE_PROPERTY(QString, _id)
    Q_PROPERTY(int id READ getid WRITE setid)
    T_DEFINE_PROPERTY(int, id)
    Q_PROPERTY(QString message READ getmessage WRITE setmessage)
    T_DEFINE_PROPERTY(QString, message)
};

#endif // MNGFORTUNEOBJECT_H
