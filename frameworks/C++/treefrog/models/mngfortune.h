#ifndef MNGFORTUNE_H
#define MNGFORTUNE_H

#include <QStringList>
#include <QDateTime>
#include <QVariant>
#include <QSharedDataPointer>
#include <TGlobal>
#include <TAbstractModel>

class TModelObject;
class MngFortuneObject;
class QJsonArray;


class T_MODEL_EXPORT MngFortune : public TAbstractModel
{
public:
    MngFortune();
    MngFortune(const MngFortune &other);
    MngFortune(const MngFortuneObject &object);
    ~MngFortune();

    int id() const;
    QString message() const;
    void setMessage(const QString &message);
    MngFortune &operator=(const MngFortune &other);

    bool create() { return TAbstractModel::create(); }
    bool update() { return TAbstractModel::update(); }
    bool save()   { return TAbstractModel::save(); }
    bool remove() { return TAbstractModel::remove(); }

    static MngFortune create(const QString &message);
    static MngFortune create(const QVariantMap &values);
    static MngFortune get(const QString &id);
    static int count();
    static QList<MngFortune> getAll();
    static QJsonArray getAllJson();

private:
    QSharedDataPointer<MngFortuneObject> d;

    TModelObject *modelData();
    const TModelObject *modelData() const;
};

Q_DECLARE_METATYPE(MngFortune)
Q_DECLARE_METATYPE(QList<MngFortune>)

#endif // MNGFORTUNE_H
