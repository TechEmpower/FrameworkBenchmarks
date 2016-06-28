#include "jsoncontroller.h"

JsonController::JsonController(const JsonController &)
    : ApplicationController()
{ }

void JsonController::index()
{
    // write codes
}

void JsonController::json()
{
    QVariantMap obj;
    obj["message"] = "Hello, World!";
    renderJson(obj);
}

// Don't remove below this line
T_REGISTER_CONTROLLER(jsoncontroller)
