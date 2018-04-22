#include "fortunecontroller.h"
#include "fortune.h"
#include "mngfortune.h"

static bool caseSensitiveLessThan(const Fortune &f1, const Fortune &f2)
{
    return f1.message() < f2.message();
}

static bool caseSensitiveMngFortuneLessThan(const MngFortune &f1, const MngFortune &f2)
{
    return f1.message() < f2.message();
}


void FortuneController::index()
{
    QList<Fortune> fortuneList = Fortune::getAll();
    Fortune fortune;
    fortune.setMessage(QLatin1String("Additional fortune added at request time."));
    fortuneList << fortune;
    // Sort
    qSort(fortuneList.begin(), fortuneList.end(), caseSensitiveLessThan);
    texport(fortuneList);
    render();
}

void FortuneController::mindex()
{
    QList<MngFortune> fortuneList = MngFortune::getAll();
    MngFortune fortune;
    fortune.setMessage(QLatin1String("Additional fortune added at request time."));
    fortuneList << fortune;
    // Sort
    qSort(fortuneList.begin(), fortuneList.end(), caseSensitiveMngFortuneLessThan);
    texport(fortuneList);
    render("mindex");
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
T_DEFINE_CONTROLLER(FortuneController)
