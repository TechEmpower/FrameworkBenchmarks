#ifndef WORLDOBJECT_H
#define WORLDOBJECT_H

#include <TSqlObject>
#include <QSharedData>


class T_MODEL_EXPORT WorldObject : public TSqlObject, public QSharedData
{
public:
    uint id;
    int randomnumber;

    enum PropertyIndex {
        Id = 0,
        Randomnumber,
    };

    QString tableName() const { return QLatin1String("World"); }
    int primaryKeyIndex() const { return Id; }
    int autoValueIndex() const { return Id; }

private:    /*** Don't modify below this line ***/
    Q_OBJECT
    Q_PROPERTY(uint id READ getid WRITE setid)
    T_DEFINE_PROPERTY(uint, id)
    Q_PROPERTY(int randomnumber READ getrandomnumber WRITE setrandomnumber)
    T_DEFINE_PROPERTY(int, randomnumber)
};

#endif // WORLDOBJECT_H
