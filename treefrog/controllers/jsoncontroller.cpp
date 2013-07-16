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

    setContentType("application/json");
    renderText(jsonEncode(obj), false);
}

// Don't remove below this line
T_REGISTER_CONTROLLER(jsoncontroller)
