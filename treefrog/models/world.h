#ifndef WORLD_H
#define WORLD_H

#include <QStringList>
#include <QDateTime>
#include <QVariant>
#include <QSharedDataPointer>
#include <TGlobal>
#include <TAbstractModel>

class TModelObject;
class WorldObject;


class T_MODEL_EXPORT World : public TAbstractModel
{
public:
    World();
    World(const World &other);
    World(const WorldObject &object);
    ~World();

    uint id() const;
    int randomNumber() const;
    void setRandomNumber(int randomNumber);
    World &operator=(const World &other);

    static World create(int randomNumber);
    static World create(const QVariantMap &values);
    static World get(uint id);
    static QList<World> getAll();

private:
    QSharedDataPointer<WorldObject> d;

    TModelObject *modelData();
    const TModelObject *modelData() const;
};

Q_DECLARE_METATYPE(World)
Q_DECLARE_METATYPE(QList<World>)

#endif // WORLD_H
