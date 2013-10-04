#ifndef JSONCONTROLLER_H
#define JSONCONTROLLER_H

#include "applicationcontroller.h"


class T_CONTROLLER_EXPORT JsonController : public ApplicationController
{
    Q_OBJECT
public:
    JsonController() { }
    JsonController(const JsonController &other);
    bool sessionEnabled() const { return false; }
    bool transactionEnabled() const { return false; }

public slots:
    void index();
    void json();
};

T_DECLARE_CONTROLLER(JsonController, jsoncontroller)

#endif // JSONCONTROLLER_H
