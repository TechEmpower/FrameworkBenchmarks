#ifndef FORTUNEOBJECT_H
#define FORTUNEOBJECT_H

#include <TSqlObject>
#include <QSharedData>


class T_MODEL_EXPORT FortuneObject : public TSqlObject, public QSharedData
{
public:
    uint id;
    QString message;

    enum PropertyIndex {
        Id = 0,
        Message,
    };

    int primaryKeyIndex() const { return Id; }
    int autoValueIndex() const { return Id; }
    QString tableName() const { return QLatin1String("Fortune"); }

private:    /*** Don't modify below this line ***/
    Q_OBJECT
    Q_PROPERTY(uint id READ getid WRITE setid)
    T_DEFINE_PROPERTY(uint, id)
    Q_PROPERTY(QString message READ getmessage WRITE setmessage)
    T_DEFINE_PROPERTY(QString, message)
};

#endif // FORTUNEOBJECT_H
