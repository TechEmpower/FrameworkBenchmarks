#ifndef FORTUNE_H
#define FORTUNE_H

#include <QStringList>
#include <QDateTime>
#include <QVariant>
#include <QSharedDataPointer>
#include <TGlobal>
#include <TAbstractModel>

class TSqlObject;
class FortuneObject;


class T_MODEL_EXPORT Fortune : public TAbstractModel
{
public:
    Fortune();
    Fortune(const Fortune &other);
    Fortune(const FortuneObject &object);
    ~Fortune();

    uint id() const;
    QString message() const;
    void setMessage(const QString &message);
    Fortune &operator=(const Fortune &other);

    static Fortune create(const QString &message);
    static Fortune create(const QVariantMap &values);
    static Fortune get(const uint &id);
    static QList<Fortune> getAll();

private:
    QSharedDataPointer<FortuneObject> d;

    TSqlObject *data();
    const TSqlObject *data() const;
};

Q_DECLARE_METATYPE(Fortune)
Q_DECLARE_METATYPE(QList<Fortune>)

#endif // FORTUNE_H
