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

QString ApplicationController::jsonEncode(const QVariantMap &obj)
{
    QString ret("{");
    for (QMap<QString, QVariant>::const_iterator i = obj.begin(); i != obj.end(); ++i) {
        ret += QString("\"%1\": \"%2\", ").arg(i.key()).arg(i.value().toString());
    }
    ret.chop(2);
    ret += QLatin1Char('}');
    return ret;
}

QString ApplicationController::jsonEncode(const QList<QVariantMap> &lst)
{
    QString ret("[");
    for (QListIterator<QVariantMap> it(lst); it.hasNext(); ) {
      ret += jsonEncode(it.next());
      ret += QLatin1String(", ");
    }
    ret.chop(2);
    ret += QLatin1Char(']');
    return ret;
}

// Don't remove below this line
T_REGISTER_CONTROLLER(applicationcontroller)
