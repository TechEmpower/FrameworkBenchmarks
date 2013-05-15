#include "fortunecontroller.h"
#include "fortune.h"


FortuneController::FortuneController(const FortuneController &)
    : ApplicationController()
{ }

void FortuneController::index()
{
    QList<Fortune> fortuneList = Fortune::getAll();
    texport(fortuneList);
    render();
}

void FortuneController::show(const QString &pk)
{
    Fortune fortune = Fortune::get(pk.toUInt());
    texport(fortune);
    render();
}

void FortuneController::entry()
{
    renderEntry();
}

void FortuneController::create()
{
    if (httpRequest().method() != Tf::Post) {
        return;
    }

    QVariantMap form = httpRequest().formItems("fortune");
    Fortune fortune = Fortune::create(form);
    if (!fortune.isNull()) {
        QString notice = "Created successfully.";
        tflash(notice);
        redirect(urla("show", fortune.id()));
    } else {
        QString error = "Failed to create.";
        texport(error);
        renderEntry(form);
    }
}

void FortuneController::renderEntry(const QVariantMap &fortune)
{
    texport(fortune);
    render("entry");
}

void FortuneController::edit(const QString &pk)
{
    Fortune fortune = Fortune::get(pk.toUInt());
    if (!fortune.isNull()) {
        renderEdit(fortune.toVariantMap());
    } else {
        redirect(urla("entry"));
    }
}

void FortuneController::save(const QString &pk)
{
    if (httpRequest().method() != Tf::Post) {
        return;
    }

    QString error;
    Fortune fortune = Fortune::get(pk.toUInt());
    if (fortune.isNull()) {
        error = "Original data not found. It may have been updated/removed by another transaction.";
        tflash(error);
        redirect(urla("edit", pk));
        return;
    }

    QVariantMap form = httpRequest().formItems("fortune");
    fortune.setProperties(form);
    if (fortune.save()) {
        QString notice = "Updated successfully.";
        tflash(notice);
        redirect(urla("show", pk));
    } else {
        error = "Failed to update.";
        texport(error);
        renderEdit(form);
    }
}

void FortuneController::renderEdit(const QVariantMap &fortune)
{
    texport(fortune);
    render("edit");
}

void FortuneController::remove(const QString &pk)
{
    if (httpRequest().method() != Tf::Post) {
        return;
    }

    Fortune fortune = Fortune::get(pk.toUInt());
    fortune.remove();
    redirect(urla("index"));
}


// Don't remove below this line
T_REGISTER_CONTROLLER(fortunecontroller)
