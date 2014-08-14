#ifndef FORTUNECONTROLLER_H
#define FORTUNECONTROLLER_H

#include "applicationcontroller.h"


class T_CONTROLLER_EXPORT FortuneController : public ApplicationController
{
    Q_OBJECT
public:
    FortuneController() { }
    FortuneController(const FortuneController &other);
    bool sessionEnabled() const { return false; }
    bool transactionEnabled() const { return false; }

public slots:
    void index();
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

T_DECLARE_CONTROLLER(FortuneController, fortunecontroller)

#endif // FORTUNECONTROLLER_H
