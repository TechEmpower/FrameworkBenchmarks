#ifndef PWORLDOBJECT_H
#define PWORLDOBJECT_H

#include <TSqlObject>
#include <QSharedData>


class T_MODEL_EXPORT PWorldObject : public TSqlObject, public QSharedData
{
public:
    uint id {0};
    int randomnumber {0};

    enum PropertyIndex {
        Id = 0,
        Randomnumber,
    };

    int primaryKeyIndex() const override { return Id; }
    int autoValueIndex() const override { return Id; }
    QString tableName() const override { return QStringLiteral("world"); }

private:    /*** Don't modify below this line ***/
    Q_OBJECT
    Q_PROPERTY(uint id READ getid WRITE setid)
    T_DEFINE_PROPERTY(uint, id)
    Q_PROPERTY(int randomnumber READ getrandomnumber WRITE setrandomnumber)
    T_DEFINE_PROPERTY(int, randomnumber)
};

#endif // PWORLDOBJECT_H
