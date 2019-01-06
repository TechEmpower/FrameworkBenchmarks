[![Download OfficeFloor](https://a.fsdn.com/con/app/sf-download-button)](https://sourceforge.net/projects/officefloor/files/latest/download)

# OfficeFloor Benchmarking Test

OfficeFloor provides true inversion of control.

> Inversion of Control = Dependency Injection + Continuation Injection + Thread Injection

More information can be found at [http://officefloor.net](http://officefloor.net)


# OfficeFloor Background

OfficeFloor completes inversion of control by adding two new paradigms:

* **Continuation Injection**: to inject functions to orchestrate application behaviour
* **Thread Injection**: to inject/select thread (pools) to execute particular functions
* *Dependency (State) Injection*: to inject objects for state into functions (currently only paradigm implemented by "inversion of control" frameworks)
 
In doing this, OfficeFloor is capable of running different threading models (e.g. both asynchronous single threaded and synchronous multi-threaded).  In actual fact, OfficeFloor opens up mixing the threading models within the application and even introduces ability for taking advantage of thread affinity to CPUs.

This follows OfficeFloor modeling people in an office environment.  As per the paper [OfficeFloor: using office patterns to improve software design](http://doi.acm.org/10.1145/2739011.2739013) ( [free download here](http://www.officefloor.net/about.html) ), OfficeFloor follows:

* Office being an application that makes decisions on information
* Tasks within the Office as functions/methods (weaved together with *Continuation Injection*)
* Office employees/workers as threads that undertake the functions/methods (assigned via *Thread Injection*)
* Forms being the objects (manage state via *Dependency Injection*)

This allows OfficeFloor to better align to how business processes actually work:

* Workers synchronously working through tasks/functions of the processes
* Workers working asynchronously with each other

In other words, people think/behave synchronously but organise asynchronously.  Hence, both thread models are in play in modelling business processes.  Furthermore, OfficeFloor makes development of asynchronous applications easier.  This is achieved by allowing the developer to avoid asynchronous coding by having synchronous functions co-ordinated asynchronously (just like workers above).

Further to this, graphical configuration is used.  The configuration for the *officefloor* benchmark test is the following:

![Graphical Configuration](configuration.png "OfficeFloor graphical configuration")


# OfficeFloor Benchmark Tests

OfficeFloor can use different HTTP server components:

* **officefloor-netty** : incorporating Netty to service requests passing to OfficeFloor inversion of control.  Benchmark test with mature HTTP solution.
* **officefloor-rapidoid** : similar to Netty but using Rapidoid.  Benchmark test with highly optimised HTTP solution.
* **officefloor-raw** : default HTTP server component provided by OfficeFloor.  This allows comparing OfficeFloor's default HTTP implementation with other solutions focused on HTTP optimisation.  It is also able to process requests concurrently on the same connection for improved responsiveness.

Having these comparisons allows developers to see the trade-offs in using different HTTP components to handle HTTP request servicing.

Note: OfficeFloor's web plugins are called WoOF (Web on OfficeFloor).

As mentioned, OfficeFloor uses different threading models.  It does not inherit the threading model imposed by the HTTP component.  Hence, there are various threading models tested to see trade-offs:

* **officefloor-tpr** : typical synchronous multi-threaded model with connections retrieved from connection pool
* **officefloor-micro** : synchronous multi-threaded model with connections bound to threads (avoids connection pool bottleneck)
* **officefloor-thread_affinity** : similar to micro, except thread pools are localised onto a particular CPU.  Hence all processing for a request is done on the same CPU (allowing much better instruction cache hits).  This effectively allows running without any synchronising of threads for potentially increased throughput.

Note: the OfficeFloor team are working with [PostgreSql ADBA](https://github.com/pgjdbc/pgadba) for asynchronous database interaction via asynchronous threading model.

While much focus is on HTTP handling, performance also is impacted by database interaction.  The above tests use raw SQL queries to provide optimised through put.  However, in "real world" applications ORMs are typically used:

* **officefloor** : WoOF implementation using EntityManager
* **officefloor-spring_data** : WoOF implementation taking advantage of Spring Data repositories.  This also demonstrates integrating Spring as a library of objects for dependency injection via OfficeFloor into functions/methods.

**officefloor-spring_data** (with possibly **officefloor-netty** incorporated) is likely representative of application development, as provides the mature easier abstractions for the developer to work with (and less code to managed).  However, the above tests show the trade-off costs in performance for the easier development.


# OfficeFloor real benefit

OfficeFloor is targeted at cloud and SOA/micro-service environments.   In these environments, applications do not operate in isolation with a single database.  Applications need to interact with multiple other services that result in:

* *synchronous threading models* : starving new requests of a thread if all threads in the pool tie up on a slow downstream system
* *aynchronous single threaded* : hard to write and hard to get right (so OfficeFloor allows synchronous code co-ordinated asynchronously for faster development and faster performance)

Furthermore, OfficeFloor handles this by using separate thread pools per downstream systems.  This way if a down stream system goes slow, only threads of that thread pool tie up.  Requests using other downstream resources continue to be serviced by the separated thread pools.

This leads to a more robust and responsive experience for the user.

Unfortunately, benchmark test coverage is yet to highlight this advantage of OfficeFloor.  However, if OfficeFloor can not perform as a typical web application, this robustness and responsiveness is mute against the sheer performance capabilities of frameworks managed by cloud elastic scale solutions.

