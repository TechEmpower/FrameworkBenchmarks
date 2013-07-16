#ifndef APPLICATIONCONTROLLER_H
#define APPLICATIONCONTROLLER_H

#include <TActionController>
#include <QVariantMap>
#include <QList>
#include "applicationhelper.h"


class T_CONTROLLER_EXPORT ApplicationController : public TActionController
{
    Q_OBJECT
public:
    ApplicationController();
    ApplicationController(const ApplicationController &other);
    virtual ~ApplicationController();
    static QString jsonEncode(const QVariantMap &obj);
    static QString jsonEncode(const QList<QVariantMap> &list);

public slots:
    void staticInitialize();

protected:
    virtual bool preFilter();    
};

T_DECLARE_CONTROLLER(ApplicationController, applicationcontroller)

#endif // APPLICATIONCONTROLLER_H
