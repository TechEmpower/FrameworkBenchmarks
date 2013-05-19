(ns hello.test.handler
  (:use clojure.test
        ring.mock.request
        hello.handler))

(deftest test-app
  (testing "main route"
    (let [response (app (request :get "/"))]
      (is (= (:status response) 200))
      (is (= (:body response)
             "<html>\n    <head>\n        <title>Welcome to hello</title>\n        <link href=\"/css/screen.css\" rel=\"stylesheet\" type=\"text/css\"></link>\n    </head>\n    <body>\n        <div class=\"navbar navbar-fixed-top navbar-inverse\">\n            <ul class=\"nav\">\n                <li>\n                    <a href=\"/\">Home</a>\n                </li>\n                <li>\n                    <a href=\"/about\">About</a>\n                </li>\n            </ul>\n        </div>\n        <div id=\"content\">\n        <h1>Welcome to hello</h1>\n        \n<h2>Some links to get started</h2><ol><li><a href='http://www.luminusweb.net/docs/html&#95;templating.md'>HTML templating</a></li><li><a href='http://www.luminusweb.net/docs/database.md'>Accessing the database</a></li><li><a href='http://www.luminusweb.net/docs/static&#95;resources.md'>Serving static resources</a></li><li><a href='http://www.luminusweb.net/docs/responses.md'>Setting response types</a></li><li><a href='http://www.luminusweb.net/docs/routes.md'>Defining routes</a></li><li><a href='http://www.luminusweb.net/docs/middleware.md'>Adding middleware</a></li><li><a href='http://www.luminusweb.net/docs/sessions&#95;cookies.md'>Sessions and cookies</a></li><li><a href='http://www.luminusweb.net/docs/security.md'>Security</a></li><li><a href='http://www.luminusweb.net/docs/deployment.md'>Deploying the application</a></li></ol>\n\n        </div>        \n        <footer>Copyright ...</footer>\n    </body>\n</html>\n\n\n"))))

  (testing "not-found route"
    (let [response (app (request :get "/invalid"))]
      (is (= (:status response) 404)))))
