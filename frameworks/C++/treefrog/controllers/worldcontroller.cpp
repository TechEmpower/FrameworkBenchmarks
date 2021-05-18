#include "worldcontroller.h"
#include "world.h"
#include "pworld.h"
#include "mngworld.h"
#include <TCache>


void WorldController::index()
{
    QList<World> worldList = World::getAll();
    texport(worldList);
    render();
}

void WorldController::plain()
{
    setContentType(QByteArrayLiteral("text/plain"));
    renderText(QStringLiteral("Hello, World!"));
}

void WorldController::show(const QString &pk)
{
    World world = World::get(pk.toUInt());
    texport(world);
    render();
}

void WorldController::queries()
{
    queries("1");
}

void WorldController::queries(const QString &num)
{
    QVariantList worlds;
    int d = std::min(std::max(num.toInt(), 1), 500);

    for (int i = 0; i < d; ++i) {
        int id = Tf::random(1, 10000);
        worlds << World::get(id).toVariantMap();
    }
    renderJson(worlds);
}

void WorldController::cached_queries()
{
    cached_queries("1");
}

void WorldController::cached_queries(const QString &num)
{
    constexpr int SECONDS = 60 * 10;  // cache time
    QVariantList worlds;
    QVariantMap world;
    int d = std::min(std::max(num.toInt(), 1), 500);

    for (int i = 0; i < d; ++i) {
        int id = Tf::random(1, 10000);
        auto key = QByteArray::number(id);
        auto randomNumber = Tf::cache()->get(key);  // Gets from cache

        if (randomNumber.isEmpty()) {
            auto w = World::get(id);
            worlds << w.toVariantMap();
            // Cache the value
            Tf::cache()->set(key, QByteArray::number(w.randomNumber()), SECONDS);
        } else {
            world.insert("id", id);
            world.insert("randomNumber", randomNumber.toInt());
            worlds << world;
        }
    }
    renderJson(worlds);
}

void WorldController::random()
{
    int id = Tf::random(1, 10000);
    World world = World::get(id);
    renderJson(world.toVariantMap());
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

void WorldController::updates()
{
    updates("1");
}

void WorldController::updates(const QString &num)
{
    QVariantList worlds;
    int d = std::min(std::max(num.toInt(), 1), 500);
    World world;

    for (int i = 0; i < d; ++i) {
        int id = Tf::random(1, 10000);
        world = World::get(id);
        world.setRandomNumber( Tf::random(1, 10000) );
        world.update();
        worlds << world.toVariantMap();
    }
    renderJson(worlds);
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

/*
 * PostgreSQL
 */
void WorldController::prandom()
{
    int id = Tf::random(1, 10000);
    PWorld world = PWorld::get(id);
    renderJson(world.toVariantMap());
}

void WorldController::pqueries()
{
    pqueries("1");
}

void WorldController::pqueries(const QString &num)
{
    QVariantList worlds;
    int d = std::min(std::max(num.toInt(), 1), 500);

    for (int i = 0; i < d; ++i) {
        int id = Tf::random(1, 10000);
        worlds << PWorld::get(id).toVariantMap();
    }
    renderJson(worlds);
}

void WorldController::cached_pqueries()
{
    cached_pqueries("1");
}

void WorldController::cached_pqueries(const QString &num)
{
    constexpr int SECONDS = 60 * 10;  // cache time
    QVariantList worlds;
    QVariantMap world;
    int d = std::min(std::max(num.toInt(), 1), 500);

    for (int i = 0; i < d; ++i) {
        int id = Tf::random(1, 10000);
        auto key = QByteArray::number(id);
        auto randomNumber = Tf::cache()->get(key);  // Gets from cache

        if (randomNumber.isEmpty()) {
            auto w = PWorld::get(id);
            worlds << w.toVariantMap();
            // Cache the value
            Tf::cache()->set(key, QByteArray::number(w.randomNumber()), SECONDS);
        } else {
            world.insert("id", id);
            world.insert("randomnumber", randomNumber.toInt());
            worlds << world;
        }
    }
    renderJson(worlds);
}

void WorldController::pupdates(const QString &num)
{
    QVariantList worlds;
    int d = std::min(std::max(num.toInt(), 1), 500);
    PWorld world;

    for (int i = 0; i < d; ++i) {
        int id = Tf::random(1, 10000);
        world = PWorld::get(id);
        world.setRandomNumber( Tf::random(1, 10000) );
        world.update();
        worlds << world.toVariantMap();
    }
    renderJson(worlds);
}

void WorldController::pupdates()
{
    pupdates("1");
}

/*
 * MongoDB
 */
void WorldController::mqueries()
{
    mqueries("1");
}

void WorldController::mqueries(const QString &num)
{
    QVariantList worlds;
    int d = std::min(std::max(num.toInt(), 1), 500);

    for (int i = 0; i < d; ++i) {
        QString id = QString::number(Tf::random(1, 10000));
        worlds << MngWorld::get(id).toVariantMap();
    }
    renderJson(worlds);
}

void WorldController::cached_mqueries()
{
    cached_mqueries("1");
}

void WorldController::cached_mqueries(const QString &num)
{
    constexpr int SECONDS = 60 * 10;  // cache time
    QVariantList worlds;
    QVariantMap world;
    int d = std::min(std::max(num.toInt(), 1), 500);

    for (int i = 0; i < d; ++i) {
        int id = Tf::random(1, 10000);
        QByteArray key = QByteArray::number(id);
        auto randomNumber = Tf::cache()->get(key);  // Gets from cache

        if (randomNumber.isEmpty()) {
            auto w = MngWorld::get(key);
            worlds << w.toVariantMap();
            // Cache the value
            Tf::cache()->set(key, QByteArray::number(w.randomNumber()), SECONDS);
        } else {
            world.insert("id", id);
            world.insert("randomNumber", randomNumber.toInt());
            worlds << world;
        }
    }
    renderJson(worlds);
}

void WorldController::mrandom()
{
    QString id = QString::number(Tf::random(1, 10000));
    auto world = MngWorld::get(id);
    renderJson(world.toVariantMap());
}

void WorldController::mupdates()
{
    mupdates("1");
}

void WorldController::mupdates(const QString &num)
{
    QVariantList worlds;
    int d = std::min(std::max(num.toInt(), 1), 500);
    MngWorld world;

    for (int i = 0; i < d; ++i) {
        QString id = QString::number(Tf::random(1, 10000));
        world = MngWorld::get(id);
        world.setRandomNumber( Tf::random(1, 10000) );
        world.update();
        worlds << world.toVariantMap();
    }
    renderJson(worlds);
}

// Don't remove below this line
T_DEFINE_CONTROLLER(WorldController)
