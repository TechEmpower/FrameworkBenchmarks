#ifndef WORLDOBJECT_H
#define WORLDOBJECT_H

#include <TSqlObject>
#include <QSharedData>


class T_MODEL_EXPORT WorldObject : public TSqlObject, public QSharedData
{
public:
    uint id;
    int randomNumber;

    enum PropertyIndex {
        Id = 0,
        RandomNumber,
    };

    int primaryKeyIndex() const { return Id; }
    int autoValueIndex() const { return Id; }
    QString tableName() const { return QLatin1String("World"); }

private:    /*** Don't modify below this line ***/
    Q_OBJECT
    Q_PROPERTY(uint id READ getid WRITE setid)
    T_DEFINE_PROPERTY(uint, id)
    Q_PROPERTY(int randomNumber READ getrandomNumber WRITE setrandomNumber)
    T_DEFINE_PROPERTY(int, randomNumber)
};

#endif // WORLDOBJECT_H
