#include "jsoncontroller.h"


void JsonController::index()
{
    // write codes
}

void JsonController::json()
{
    static QJsonObject obj {{"message", "Hello, World!"}};
    renderJson(obj);
}

// Don't remove below this line
T_DEFINE_CONTROLLER(JsonController)
