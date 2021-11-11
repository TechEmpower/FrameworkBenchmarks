#include <signal.h>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>
#include <algorithm>

#include <Wt/WServer.h>
#include <Wt/WResource.h>
#include <Wt/Http/Request.h>
#include <Wt/Http/Response.h>
#include <Wt/WTemplate.h>
#include <Wt/Utils.h>

#include <Wt/Dbo/Dbo.h>
#include <Wt/Dbo/Json.h>
#ifndef BENCHMARK_USE_POSTGRES
#include <Wt/Dbo/backend/MySQL.h>
#else
#include <Wt/Dbo/backend/Postgres.h>
#endif

#include <random>

#ifndef WT_WIN32
extern char **environ;
#endif // WT_WIN32

class MyMessage {
public:
  std::string message;

  template<class Action>
  void persist(Action& a) {
    Wt::Dbo::field(a, message, "message");
  }
};

class World {
public:
  int randomNumber;

  template<class Action>
  void persist(Action& a) {
    Wt::Dbo::field(a, randomNumber, "randomnumber");
  }
};

class Fortune {
public:
  std::string message;

  template<class Action>
  void persist(Action& a) {
    Wt::Dbo::field(a, message, "message");
  }
};

namespace Wt {
  namespace Dbo {
    template<>
    struct dbo_traits<World> : public dbo_default_traits {
      static const char *versionField() {
        return 0;
      }
      static IdType invalidId() {
        return 0;
      }
    };
    template<>
    struct dbo_traits<Fortune> : public dbo_default_traits {
      static const char *versionField() {
        return 0;
      }
      static IdType invalidId() {
        return 0;
      }
    };
  }
}

class JsonResource : public Wt::WResource {
public:
    virtual void handleRequest(const Wt::Http::Request &request, Wt::Http::Response &response) {
        response.setMimeType("application/json");
        response.addHeader("Server", "Wt");

        MyMessage message;
        message.message = "Hello, World!";

        Wt::Dbo::JsonSerializer writer(response.out());
        writer.serialize(message);
    }
};

class MyConnection : public
#ifdef BENCHMARK_USE_POSTGRES
  Wt::Dbo::backend::Postgres
#else
  Wt::Dbo::backend::MySQL
#endif
{
public:
#ifdef BENCHMARK_USE_POSTGRES
  MyConnection(const std::string &db) :
  Wt::Dbo::backend::Postgres(db) {}
#else
  MyConnection(const std::string &db, const std::string &dbuser, const std::string &dbpasswd, const std::string &dbhost, unsigned int dbport) :
  Wt::Dbo::backend::MySQL(db, dbuser, dbpasswd, dbhost, dbport) {}
#endif

  virtual void startTransaction() { }
  virtual void commitTransaction() { }
  virtual void rollbackTransaction() { }
};

struct DbStruct {
  MyConnection *connection;
  Wt::Dbo::Session session;

  std::default_random_engine rng;
  std::uniform_int_distribution<int> distribution;

  DbStruct()
    : connection(0),
      rng(clock()),
      distribution(1, 10000) {
    std::string dbHostStr = "localhost";
    char *dbHost = std::getenv("DBHOST");
    if (dbHost)
      dbHostStr = std::string(dbHost);
#ifndef BENCHMARK_USE_POSTGRES
    auto c = Wt::cpp14::make_unique<MyConnection>("hello_world", "benchmarkdbuser", "benchmarkdbpass", dbHostStr, 3306);
#else
    auto connStr = std::string("host=") + dbHostStr + " port=5432 user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world";
    auto c = Wt::cpp14::make_unique<MyConnection>(connStr);
#endif

    connection = c.get();
    session.setConnection(std::move(c));
    session.mapClass<World>("world");
    session.mapClass<Fortune>("fortune");
  }

  int rand() {
    return distribution(rng);
  }
};

namespace {
  thread_local DbStruct *dbStruct_;
}

class DbResource : public Wt::WResource {
public:
  virtual void handleRequest(const Wt::Http::Request &request, Wt::Http::Response &response) {
    response.setMimeType("application/json");
    response.addHeader("Server", "Wt");

    if (!dbStruct_) {
      dbStruct_ = new DbStruct();
    }

    Wt::Dbo::Transaction transaction(dbStruct_->session);
    Wt::Dbo::ptr<World> entry = dbStruct_->session.load<World>(dbStruct_->rand());
    
    Wt::Dbo::JsonSerializer writer(response.out());
    writer.serialize(entry);
  }
};

class QueriesResource : public Wt::WResource {
public:
  virtual void handleRequest(const Wt::Http::Request &request, Wt::Http::Response &response) {
    int n;
    if (const std::string *queries = request.getParameter("queries")) {
      n = atoi(queries->c_str());
      if (n < 1)
        n = 1;
      else if (n > 500)
        n = 500;
    } else {
      n = 1;
    }

    response.setMimeType("application/json");
    response.addHeader("Server", "Wt");

    if (!dbStruct_) {
      dbStruct_ = new DbStruct();
    }

    Wt::Dbo::Transaction transaction(dbStruct_->session);
    std::vector<Wt::Dbo::ptr<World> > results;
    results.reserve(n);
    for (int i = 0; i < n; ++i) {
      results.push_back(dbStruct_->session.load<World>(dbStruct_->rand()));
    }
    Wt::Dbo::JsonSerializer writer(response.out());
    writer.serialize(results);
  }
};

typedef Wt::Dbo::collection< Wt::Dbo::ptr<Fortune> > Fortunes;
typedef std::vector<Wt::Dbo::ptr<Fortune> > VFortunes;

bool fortuneCmp(const Wt::Dbo::ptr<Fortune>& f1, const Wt::Dbo::ptr<Fortune>& f2) {
  return strcmp(f1->message.c_str(), f2->message.c_str()) < 0;
}

class FortuneTemplate : public Wt::WTemplate {
private:
  const VFortunes *fortunes_;
  mutable std::vector<Wt::Dbo::ptr<Fortune> >::const_iterator it_;
public:
  FortuneTemplate(const std::vector<Wt::Dbo::ptr<Fortune> >& fortunes)
    : Wt::WTemplate(tr("fortunes")),
      fortunes_(&fortunes),
      it_(fortunes.end())
  {
    addFunction("while", &Wt::WTemplate::Functions::while_f);
  }

  virtual bool conditionValue(const std::string& name) const {
    if (name == "next-fortune") {
      if (it_ == fortunes_->end())
        it_ = fortunes_->begin();
      else
        ++it_;

      if (it_ == fortunes_->end())
        return false;

      return true;
    } else
      return Wt::WTemplate::conditionValue(name);
  }

  virtual void resolveString(const std::string& varName, const std::vector<Wt::WString>& vars, std::ostream& result) {
    if (varName == "id")
      result << it_->id();
    else if (varName == "message")
      format(result, Wt::WString((*it_)->message));
    else
      Wt::WTemplate::resolveString(varName, vars, result);
  }
};

class FortuneResource : public Wt::WResource {
public:
  virtual void handleRequest(const Wt::Http::Request &request, Wt::Http::Response &response) {
    response.setMimeType("text/html; charset=utf-8");
    response.addHeader("Server", "Wt");

    if (!dbStruct_) {
      dbStruct_ = new DbStruct();
    }

    Wt::Dbo::Transaction transaction(dbStruct_->session);
    Fortunes fortunes = dbStruct_->session.find<Fortune>();
    VFortunes vFortunes;
    for (Fortunes::const_iterator i = fortunes.begin(); i != fortunes.end(); ++i)
      vFortunes.push_back(*i);
    auto additionalFortune = Wt::cpp14::make_unique<Fortune>();
    additionalFortune->message = "Additional fortune added at request time.";
    vFortunes.push_back(Wt::Dbo::ptr<Fortune>(std::move(additionalFortune)));

    std::sort(vFortunes.begin(), vFortunes.end(), fortuneCmp);

    FortuneTemplate tpl(vFortunes);

    response.out() << "<!DOCTYPE html>";
    tpl.renderTemplate(response.out());
  }
};

class UpdateResource : public Wt::WResource {
public:
  virtual void handleRequest(const Wt::Http::Request &request, Wt::Http::Response &response) {
    int n;
    if (const std::string *queries = request.getParameter("queries")) {
      n = atoi(queries->c_str());
      if (n < 1)
        n = 1;
      else if (n > 500)
        n = 500;
    } else {
      n = 1;
    }

    response.setMimeType("application/json");
    response.addHeader("Server", "Wt");

    if (!dbStruct_) {
      dbStruct_ = new DbStruct();
    }

    std::vector<Wt::Dbo::ptr<World> > results;

    for (int i = 0; i < n; ++i) {
      bool success = false;
      while (!success) {
        try {
          Wt::Dbo::Transaction transaction(dbStruct_->session);
          Wt::Dbo::ptr<World> world = dbStruct_->session.load<World>(dbStruct_->rand());
          world.modify()->randomNumber = dbStruct_->rand();
          transaction.commit();
          results.push_back(world);
          success = true;
        } catch (Wt::Dbo::Exception& e) {
          // Retry
        }
      }
    }

    Wt::Dbo::JsonSerializer writer(response.out());
    writer.serialize(results);
  }
};

class PlaintextResource : public Wt::WResource {
  virtual void handleRequest(const Wt::Http::Request &request, Wt::Http::Response &response) {
    response.setMimeType("text/plain");
    response.addHeader("Server", "Wt");

    response.out() << "Hello, World!";
  }
};

int main(int argc, char** argv) {
  try {
    Wt::WServer server(argv[0]);

    server.setServerConfiguration(argc, argv, WTHTTP_CONFIGURATION);

    auto bundle = std::make_shared<Wt::WMessageResourceBundle>();
    bundle->use(server.appRoot() + "fortunes");
    server.setLocalizedStrings(bundle);

    JsonResource jsonResource;
    server.addResource(&jsonResource, "/json");

    DbResource dbResource;
    server.addResource(&dbResource, "/db");

    QueriesResource queriesResource;
    server.addResource(&queriesResource, "/queries");

    FortuneResource fortuneResource;
    server.addResource(&fortuneResource, "/fortune");

    UpdateResource updateResource;
    server.addResource(&updateResource, "/updates");
    
    PlaintextResource plaintextResource;
    server.addResource(&plaintextResource, "/plaintext");

    if (server.start()) {
      int sig = Wt::WServer::waitForShutdown();

      std::cerr << "Shutdown (signal = " << sig << ")" << std::endl;
      server.stop();

#ifndef WT_WIN32
      if (sig == SIGHUP)
        Wt::WServer::restart(argc, argv, environ);
#endif // WT_WIN32
    }
  } catch (Wt::WServer::Exception& e) {
    std::cerr << e.what() << "\n";
    return 1;
  } catch (std::exception& e) {
    std::cerr << "exception: " << e.what() << "\n";
    return 1;
  }
}
