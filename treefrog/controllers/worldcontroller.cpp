#include "worldcontroller.h"
#include "world.h"


WorldController::WorldController(const WorldController &)
    : ApplicationController()
{ }

void WorldController::index()
{
    QList<World> worldList = World::getAll();
    texport(worldList);
    render();
}

void WorldController::plain()
{
    renderText(QLatin1String("Hello, World!"));
}

void WorldController::show(const QString &pk)
{
    World world = World::get(pk.toUInt());
    texport(world);
    render();
}

void WorldController::queries(const QString &num)
{
    QList<QVariantMap> worlds;
    int d = num.toInt();
    for (int i = 0; i < d; ++i) {
        int id = Tf::random(9999) + 1;
        World world = World::get(id);
        worlds << world.toVariantMap();
    }
    setContentType("application/json; charset=UTF-8");
    renderText(jsonEncode(worlds), false);
}

void WorldController::random()
{
    int id = Tf::random(9999) + 1;
    World world = World::get(id);
    setContentType("application/json; charset=UTF-8");
    renderText(jsonEncode(world.toVariantMap()), false);
}

void WorldController::entry()
{
    renderEntry();
}

void WorldController::create()
{
    if (httpRequest().method() != Tf::Post) {
        return;
    }

    QVariantMap form = httpRequest().formItems("world");
    World world = World::create(form);
    if (!world.isNull()) {
        QString notice = "Created successfully.";
        tflash(notice);
        redirect(urla("show", world.id()));
    } else {
        QString error = "Failed to create.";
        texport(error);
        renderEntry(form);
    }
}

void WorldController::renderEntry(const QVariantMap &world)
{
    texport(world);
    render("entry");
}

void WorldController::edit(const QString &pk)
{
    World world = World::get(pk.toUInt());
    if (!world.isNull()) {
        renderEdit(world.toVariantMap());
    } else {
        redirect(urla("entry"));
    }
}

void WorldController::save(const QString &pk)
{
    if (httpRequest().method() != Tf::Post) {
        return;
    }

    QString error;
    World world = World::get(pk.toUInt());
    if (world.isNull()) {
        error = "Original data not found. It may have been updated/removed by another transaction.";
        tflash(error);
        redirect(urla("edit", pk));
        return;
    }

    QVariantMap form = httpRequest().formItems("world");
    world.setProperties(form);
    if (world.save()) {
        QString notice = "Updated successfully.";
        tflash(notice);
        redirect(urla("show", pk));
    } else {
        error = "Failed to update.";
        texport(error);
        renderEdit(form);
    }
}

void WorldController::renderEdit(const QVariantMap &world)
{
    texport(world);
    render("edit");
}

void WorldController::updates(const QString &num)
{
    QList<QVariantMap> worlds;
    int d = num.toInt();
    for (int i = 0; i < d; ++i) {
        int id = Tf::random(9999) + 1;
        World world = World::get(id);
        world.setRandomNumber( Tf::random(9999) + 1 );
        world.update();
	worlds << world.toVariantMap();
    }
    setContentType("application/json; charset=UTF-8");
    renderText(jsonEncode(worlds), false);
}

void WorldController::remove(const QString &pk)
{
    if (httpRequest().method() != Tf::Post) {
        return;
    }

    World world = World::get(pk.toUInt());
    world.remove();
    redirect(urla("index"));
}


// Don't remove below this line
T_REGISTER_CONTROLLER(worldcontroller)
