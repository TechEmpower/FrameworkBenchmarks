#include "applicationcontroller.h"


ApplicationController::ApplicationController()
    : TActionController()
{ }

ApplicationController::ApplicationController(const ApplicationController &)
    : TActionController()
{ }

ApplicationController::~ApplicationController()
{ }

void ApplicationController::staticInitialize()
{ }

bool ApplicationController::preFilter()
{
    return true;
}

// Don't remove below this line
T_REGISTER_CONTROLLER(applicationcontroller)
