#ifndef PWORLD_H
#define PWORLD_H

#include <QStringList>
#include <QDateTime>
#include <QVariant>
#include <QSharedDataPointer>
#include <TGlobal>
#include <TAbstractModel>

class TModelObject;
class PWorldObject;


class T_MODEL_EXPORT PWorld : public TAbstractModel
{
public:
    PWorld();
    PWorld(const PWorld &other);
    PWorld(const PWorldObject &object);
    ~PWorld();

    uint id() const;
    int randomNumber() const;
    void setRandomNumber(int randomNumber);
    PWorld &operator=(const PWorld &other);

    bool create() { return TAbstractModel::create(); }
    bool update();
    bool save()   { return TAbstractModel::save(); }
    bool remove() { return TAbstractModel::remove(); }

    static PWorld create(int randomNumber);
    static PWorld create(const QVariantMap &values);
    static PWorld get(uint id);
    static int count();
    static QList<PWorld> getAll();

private:
    QSharedDataPointer<PWorldObject> d;

    TModelObject *modelData();
    const TModelObject *modelData() const;
};

Q_DECLARE_METATYPE(PWorld)
Q_DECLARE_METATYPE(QList<PWorld>)

#endif // PWORLD_H
