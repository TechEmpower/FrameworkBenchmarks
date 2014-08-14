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
    const QString JSON_OBJ_INT("\"%1\":%2, ");
    const QString JSON_OBJ_STR("\"%1\":\"%2\", ");
    QString ret("{");

    for (QMap<QString, QVariant>::const_iterator i = obj.begin(); i != obj.end(); ++i) {
        switch (i.value().type()) {
        case QVariant::UInt:
        case QVariant::Int:
            ret += JSON_OBJ_INT.arg(i.key()).arg(i.value().toInt());
            break;
        default:
            ret += JSON_OBJ_STR.arg(i.key()).arg(i.value().toString());
            break;
        }
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
