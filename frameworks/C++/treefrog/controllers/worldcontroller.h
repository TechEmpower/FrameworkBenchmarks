#ifndef WORLDCONTROLLER_H
#define WORLDCONTROLLER_H

#include "applicationcontroller.h"


class T_CONTROLLER_EXPORT WorldController : public ApplicationController
{
    Q_OBJECT
public:
    WorldController() { }
    bool sessionEnabled() const { return false; }
    bool transactionEnabled() const { return false; }

public slots:
    void index();
    void plain();
    void show(const QString &pk);
    void queries();
    void queries(const QString &num);
    void cached_queries();
    void cached_queries(const QString &num);
    void random();
    void entry();
    void create();
    void edit(const QString &pk);
    void save(const QString &pk);
    void updates(const QString &num);
    void updates();
    void remove(const QString &pk);

    // PostgreSQL
    void prandom();
    void pqueries();
    void pqueries(const QString &num);
    void cached_pqueries();
    void cached_pqueries(const QString &num);
    void pupdates(const QString &num);
    void pupdates();

    // MongoDB
    void mqueries();
    void mqueries(const QString &num);
    void cached_mqueries();
    void cached_mqueries(const QString &num);
    void mrandom();
    void mupdates(const QString &num);
    void mupdates();

private:
    void renderEntry(const QVariantMap &world = QVariantMap());
    void renderEdit(const QVariantMap &world = QVariantMap());
};

#endif // WORLDCONTROLLER_H
