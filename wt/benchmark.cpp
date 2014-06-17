#include <signal.h>
#include <cstdlib>
#include <string>
#include <vector>
#include <algorithm>
#include <mutex>

#include <Wt/WServer>
#include <Wt/WResource>
#include <Wt/Http/Request>
#include <Wt/Http/Response>
#include <Wt/WTemplate>
#include <Wt/Utils>

#include <Wt/Dbo/Dbo>
#include <Wt/Dbo/Json>
#ifndef BENCHMARK_USE_POSTGRES
#include <Wt/Dbo/backend/MySQL>
#else
#include <Wt/Dbo/backend/Postgres>
#endif

#include <boost/random/uniform_int_distribution.hpp>
#include <boost/random/taus88.hpp>
#include <boost/thread/tss.hpp>

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
    // Workaround for issue #783
    if (a.getsValue()) {
      Wt::Dbo::field(a, randomNumber, "randomNumber");
    } else {
      Wt::Dbo::field(a, randomNumber, "randomnumber");
    }
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

std::mutex mtx;

struct DbStruct {
#ifndef BENCHMARK_USE_POSTGRES
  Wt::Dbo::backend::MySQL connection;
#else
  Wt::Dbo::backend::Postgres connection;
#endif
  Wt::Dbo::Session session;
  Wt::Dbo::Transaction *transaction;

  boost::taus88 rng;
  boost::random::uniform_int_distribution<int> distribution;

#ifndef BENCHMARK_USE_POSTGRES
  DbStruct() : connection("hello_world", "benchmarkdbuser", "benchmarkdbpass", "INSERT_DB_HOST_HERE", 3306),
#else
  DbStruct() : connection("host=INSERT_DB_HOST_HERE port=5432 user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world"),
#endif
      rng(clock()),
      distribution(1, 10000) {	       
    session.setConnection(connection);
    session.mapClass<World>("world");
    session.mapClass<Fortune>("fortune");
    transaction = new Wt::Dbo::Transaction(session);
  }

  ~DbStruct() {
    delete transaction;
  }

  int rand() {
    return distribution(rng);
  }
};

struct DbStructNoTransaction {
#ifndef BENCHMARK_USE_POSTGRES
  Wt::Dbo::backend::MySQL connection;
#else
  Wt::Dbo::backend::Postgres connection;
#endif
  Wt::Dbo::Session session;

  boost::taus88 rng;
  boost::random::uniform_int_distribution<int> distribution;

#ifndef BENCHMARK_USE_POSTGRES
  DbStructNoTransaction() : connection("hello_world", "benchmarkdbuser", "benchmarkdbpass", "INSERT_DB_HOST_HERE", 3306),
#else
  DbStructNoTransaction() : connection("host=INSERT_DB_HOST_HERE port=5432 user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world"),
#endif
      rng(clock()),
      distribution(1, 10000) {
    session.setConnection(connection);
    session.mapClass<World>("world");
    session.mapClass<Fortune>("fortune");
  }

  int rand() {
    return distribution(rng);
  }
};

class DbResource : public Wt::WResource {
private:
  boost::thread_specific_ptr<DbStruct> dbStruct_;
public:
  virtual void handleRequest(const Wt::Http::Request &request, Wt::Http::Response &response) {
    response.setMimeType("application/json");
    response.addHeader("Server", "Wt");

    DbStruct* db = dbStruct_.get();
    if (!db) {
      std::lock_guard<std::mutex> lock(mtx);
      if (!db) {
	db = new DbStruct();
	dbStruct_.reset(db);
      }
    }

    Wt::Dbo::ptr<World> entry = db->session.load<World>(db->rand());
    
    Wt::Dbo::JsonSerializer writer(response.out());
    writer.serialize(entry);
  }
};

class QueriesResource : public Wt::WResource {
private:
  boost::thread_specific_ptr<DbStruct> dbStruct_;
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

    DbStruct* db = dbStruct_.get();
    if (!db) {
      std::lock_guard<std::mutex> lock(mtx);
      if (!db) {
	db = new DbStruct();
	dbStruct_.reset(db);
      }
    }

    std::vector<Wt::Dbo::ptr<World> > results;
    results.reserve(n);
    for (int i = 0; i < n; ++i) {
      results.push_back(db->session.load<World>(db->rand()));
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
  FortuneTemplate(const std::vector<Wt::Dbo::ptr<Fortune> >& fortunes) : fortunes_(&fortunes), it_(fortunes.end()), Wt::WTemplate(Wt::WString::tr("fortunes")) {
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
      format(result, Wt::WString::fromUTF8(boost::lexical_cast<std::string>(it_->id())), Wt::XHTMLUnsafeText);
    else if (varName == "message")
      format(result, Wt::WString::fromUTF8((*it_)->message));
    else
      Wt::WTemplate::resolveString(varName, vars, result);
  }
};

class FortuneResource : public Wt::WResource {
private:
  boost::thread_specific_ptr<DbStruct> dbStruct_;
public:
  virtual void handleRequest(const Wt::Http::Request &request, Wt::Http::Response &response) {
    response.setMimeType("text/html; charset=utf-8");
    response.addHeader("Server", "Wt");

    DbStruct* db = dbStruct_.get();
    if (!db) {
      std::lock_guard<std::mutex> lock(mtx);
      if (!db) {
	db = new DbStruct();
	dbStruct_.reset(db);
      }
    }

    Fortunes fortunes = db->session.find<Fortune>();
    VFortunes vFortunes;
    for (Fortunes::const_iterator i = fortunes.begin(); i != fortunes.end(); ++i)
      vFortunes.push_back(*i);
    Fortune* additionalFortune = new Fortune();
    additionalFortune->message = "Additional fortune added at request time.";
    vFortunes.push_back(Wt::Dbo::ptr<Fortune>(additionalFortune));

    std::sort(vFortunes.begin(), vFortunes.end(), fortuneCmp);

    FortuneTemplate tpl(vFortunes);

    response.out() << "<!DOCTYPE html>";
    tpl.renderTemplate(response.out());
  }
};

class UpdateResource : public Wt::WResource {
private:
  boost::thread_specific_ptr<DbStructNoTransaction> dbStruct_;
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

    DbStructNoTransaction* db = dbStruct_.get();
    if (!db) {
      std::lock_guard<std::mutex> lock(mtx);
      if (!db) {
	db = new DbStructNoTransaction();
	dbStruct_.reset(db);
      }
    }

    std::vector<Wt::Dbo::ptr<World> > results;

    for (int i = 0; i < n; ++i) {
      bool success = false;
      while (!success) {
	try {
	  Wt::Dbo::Transaction transaction(db->session);
	  Wt::Dbo::ptr<World> world = db->session.load<World>(db->rand());
	  world.modify()->randomNumber = db->rand();
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
    Wt::WMessageResourceBundle *bundle = new Wt::WMessageResourceBundle();
    bundle->use("fortunes");
    server.setLocalizedStrings(bundle);

    server.setServerConfiguration(argc, argv, WTHTTP_CONFIGURATION);

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
      int sig = Wt::WServer::waitForShutdown(argv[0]);

      std::cerr << "Shutdown (signal = " << sig << ")" << std::endl;
      server.stop();

      if (sig == SIGHUP)
        Wt::WServer::restart(argc, argv, environ);
    }
  } catch (Wt::WServer::Exception& e) {
    std::cerr << e.what() << "\n";
    return 1;
  } catch (std::exception& e) {
    std::cerr << "exception: " << e.what() << "\n";
    return 1;
  }
}
