// This file is a part of the IncludeOS unikernel - www.includeos.org
//
// Copyright 2015-2016 Oslo and Akershus University College of Applied Sciences
// and Alfred Bratterud
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <service>
#include <os>
#include <net/inet4>
#include <mana/server.hpp>

using namespace mana;
using namespace std::string_literals;

std::unique_ptr<mana::Server> server_;

void Service::start(const std::string&)
{
  /** IP STACK SETUP **/
  // Bring up IPv4 stack on network interface 0
  auto& stack = net::Inet4::ifconfig(5.0,
    [] (bool timeout) {
      printf("DHCP resolution %s\n", timeout ? "failed" : "succeeded");
      if (timeout)
      {
        /**
         * Default Manual config. Can only be done after timeout to work
         * with DHCP offers going to unicast IP (e.g. in GCE)
         **/
        net::Inet4::stack().network_config({ 10,0,0,42 },     // IP
                                           { 255,255,255,0 }, // Netmask
                                           { 10,0,0,1 },      // Gateway
                                           { 8,8,8,8 });      // DNS
      }
    });

  // Create a router
  Router router;
  // Setup a route on GET /
  router.on_get("/plaintext", [](auto, auto res) {
    res->source().set_status_code(http::OK);
    res->header().add_field(http::header::Server, "IncludeOS/" + OS::version());
    res->header().add_field(http::header::Content_Type, "text/plain");
    res->header().add_field(http::header::Date, "Mon, 01 Jan 1970 00:00:01 GMT");
    res->source().add_body("Hello, world!"s);
    res->send();
  });
  INFO("Router", "Registered routes:\n%s", router.to_string().c_str());

  // Create and setup the server
  server_ = std::make_unique<Server>(stack.tcp());
  server_->set_routes(router).listen(80);

}
