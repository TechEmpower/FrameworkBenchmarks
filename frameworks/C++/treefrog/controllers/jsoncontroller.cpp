#include "jsoncontroller.h"


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
T_DEFINE_CONTROLLER(JsonController)
