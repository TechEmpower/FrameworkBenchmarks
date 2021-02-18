# OfficeFloor

OfficeFloor provides true inversion of (coupling) control.

> Inversion of Control = Dependency Injection + Continuation Injection + Thread Injection

OfficeFloor completes inversion of control by adding two new paradigms:

* **Continuation Injection**: to inject functions to orchestrate application behaviour
* **Thread Injection**: to inject/select thread (pools) to execute particular functions
* *Dependency (State) Injection*: to inject objects for state into functions (currently only paradigm implemented by "inversion of control" frameworks)
 
In doing this, OfficeFloor is capable of running different threading models (e.g. both asynchronous single threaded and synchronous multi-threaded).  In actual fact, OfficeFloor opens up mixing the threading models within the application and even introduces ability for taking advantage of thread affinity to CPUs.

This follows OfficeFloor modeling people in an office environment:

* Office being an application that makes decisions on information
* Tasks within the Office as functions/methods (weaved together with *Continuation Injection*)
* Office employees/workers as threads that undertake the functions/methods (assigned via *Thread Injection*)
* Forms being the objects (manage state via *Dependency Injection*)

This allows OfficeFloor to better align to how business processes actually work:

* Workers synchronously working through tasks/functions of the processes
* Workers working asynchronously with each other

In other words, people think/behave synchronously but organise asynchronously.  Hence, both thread models are in play in modeling business processes.  Furthermore, OfficeFloor makes development of asynchronous applications easier.  This is achieved by allowing the developer to avoid asynchronous coding by having synchronous functions co-ordinated asynchronously (just like workers above).

Further to this, graphical configuration is used.  The configuration for the *officefloor* benchmark test is the following:

![Graphical Configuration](configuration.png "OfficeFloor graphical configuration")

More information can be found at [http://officefloor.net](http://officefloor.net)


# OfficeFloor Benchmark Tests

OfficeFloor can use different HTTP server components:

* **officefloor-netty** : incorporating Netty to service requests passing to OfficeFloor inversion of control.
* **officefloor-undertow** : incorporating Undertow to service requests passing to OfficeFloor inversion of control.
* **officefloor-raw** : default HTTP server component provided by OfficeFloor.  This allows comparing OfficeFloor's raw HTTP implementation with other solutions focused on HTTP optimisation.

Having these comparisons allows developers to see the trade-offs in using different HTTP components to handle HTTP request servicing.

Note: OfficeFloor's web plugins are called WoOF (Web on OfficeFloor).

As mentioned, OfficeFloor uses different threading models.  It does not inherit the threading model imposed by the HTTP component.  Hence, there are various threading models tested to see trade-offs:

* **officefloor-async** : asynchronous single-threaded
* **officefloor-micro** : synchronous multi-threaded model
* **officefloor-thread_affinity** : thread affinity of servicing each request only on one CPU

While much focus is on HTTP handling, performance also is impacted by database interaction.  The above tests use raw SQL queries to provide optimised through put.  However, in "real world" applications ORMs are typically used:

* **officefloor** : uses JPA
* **officefloor-spring_data** : uses Spring Data repositories


# 503 Responses

 Within the performance tests (particularly the query and update tests), there are 503 responses from OfficeFloor.  These are deliberate and make OfficeFloor significantly more responsive than typical web servers.  To understand why, we need to explain the servicing change introduced by OfficeFloor.
 
 **Typical web servers** manage load by only processing one request at a time per socket.  This means on pipelined requests over a single connection, the first request needs to be fully processed before the next request is started.  As requests then queue on the network buffer, it allows the network's natural TCP throttling to manage load.
 
 **Web browser request profiles have changed** to an increased number of asynchronous requests pipelined over a few number of connections.  Days of old (possibly very old), web pages loaded and then did a post to reload the page again.  There would be one page submission with a few resource requests (e.g. images).  These days, single page applications are a lot more prevalent.  The nature of single page applications is to avoid page reloads and run many asynchronous requests.  This means more logic submission requests in parallel.  As browsers re-use connections to multiplex these requests to the server, the result is pipelining requests over a few number of connections.
 
 **OfficeFloor processes pipelined requests in parallel**.  To cater to this situation and allow more responsive single page web applications, OfficeFloor processes the pipeline requests in parallel.  This means the second request in the pipeline no longer has to wait for the first one to complete.  Both the first, second, third, etc will be processed immediately by OfficeFloor.  This means better responsiveness to the single page applications, as there is no queuing of requests in the pipelines.
 
 **Load throttling managed by 503 responses.**  As requests are no longer queued on the network buffer, there is no TCP throttling.  Requests will be accepted in parallel into OfficeFloor.  To avoid denial of service attacks, OfficeFloor has built in load throttling.  It is beyond this overview discussion to explain how this works, but basically when thread pools are overloaded they reject new jobs.  OfficeFloor captures these rejections and translates them into 503 HTTP responses (indicating temporarily unavailable).
 
 **This improves responsiveness of single page applications**.  The problem with TCP throttling is that requests start to slow down before you get rejections.  This means your single page application ends up waiting for responses.  The wait time is noticeable by the user, as can be upwards of 10's of seconds to minutes.  This then results in the user's perception of the single page application being slow.  OfficeFloor by parallel processing everything and responding immediately with 503 responses when overloaded, reduces these long wait times.  Rather than than hanging, the single page application can deal with the 503 by calling another server or providing user friendly messages back to the user that system is under load and try back in a few moments.  This provides an overall more responsive and what we find is a better experience for the user.
 
 However, this does mean in some of the heavier load tests, you will see errors for OfficeFloor.  But we here at OfficeFloor are proud of these errors, as they avoid hanging applications and provide better user experience under extreme loads.


# OfficeFloor real benefit

OfficeFloor is targeted at cloud and SOA/micro-service environments.   In these environments, applications do not operate in isolation with a single database.  Applications need to interact with multiple other services that result in:

* *synchronous threading models* : starving new requests of a thread if all threads in the pool tie up on a slow downstream system
* *aynchronous single threaded* : hard to write and hard to get right (so OfficeFloor allows synchronous code co-ordinated asynchronously for faster development and faster performance)

Furthermore, OfficeFloor handles this by using separate thread pools per downstream systems.  This way if a down stream system goes slow, only threads of that thread pool tie up.  Requests using other downstream resources continue to be serviced by the separated thread pools.

This leads to a more robust and responsive experience for the user.

Unfortunately, benchmark test coverage is yet to highlight this advantage of OfficeFloor.  However, if OfficeFloor can not perform as a typical web application, this robustness and responsiveness is mute against the sheer performance capabilities of frameworks managed by cloud elastic scale solutions.

