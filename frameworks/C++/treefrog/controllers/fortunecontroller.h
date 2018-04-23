#ifndef FORTUNECONTROLLER_H
#define FORTUNECONTROLLER_H

#include "applicationcontroller.h"


class T_CONTROLLER_EXPORT FortuneController : public ApplicationController
{
    Q_OBJECT
public:
    FortuneController() { }
    bool sessionEnabled() const { return false; }
    bool transactionEnabled() const { return false; }

public slots:
    void index();   // SQL
    void mindex();  // MongoDB
    void show(const QString &pk);
    void entry();
    void create();
    void edit(const QString &pk);
    void save(const QString &pk);
    void remove(const QString &pk);

private:
    void renderEntry(const QVariantMap &fortune = QVariantMap());
    void renderEdit(const QVariantMap &fortune = QVariantMap());
};

#endif // FORTUNECONTROLLER_H
