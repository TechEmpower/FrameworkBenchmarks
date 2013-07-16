#ifndef WORLDCONTROLLER_H
#define WORLDCONTROLLER_H

#include "applicationcontroller.h"


class T_CONTROLLER_EXPORT WorldController : public ApplicationController
{
    Q_OBJECT
public:
    WorldController() { }
    WorldController(const WorldController &other);
    bool sessionEnabled() const { return false; }
    bool transactionEnabled() const { return false; }

public slots:
    void index();
    void show(const QString &pk);
    void queries(const QString &num);
    void random();
    void entry();
    void create();
    void edit(const QString &pk);
    void save(const QString &pk);
    void remove(const QString &pk);

private:
    void renderEntry(const QVariantMap &world = QVariantMap());
    void renderEdit(const QVariantMap &world = QVariantMap());
};

T_DECLARE_CONTROLLER(WorldController, worldcontroller)

#endif // WORLDCONTROLLER_H
