# Top-level macros for eaiser development.
#
# # Cheatsheet:
#
# `require "onyx/env"` to read from `.env.*`, `.env.*.local` variables.
# `require "onyx/logger"` to enable `Onyx.logger`
# `require "onyx/db"` to enable `Onyx.db`
#
# ## HTTP
#
# `require "onyx/http"` to enable the top-level HTTP server instance and these methods:
#
# * `Onyx.get`, `Onyx.post`, `Onyx.put`, `Onyx.patch`, `Onyx.delete` and `Onyx.options`
# * `Onyx.draw`
# * `Onyx.listen`
#
# ## REST
#
# `require "onyx/rest"` to enable the top-level REST server instance and `Onyx.render`.
# This *includes* `"onyx/http"`, so you don't need to `require` it explicitly
#
# ## SQL
#
# `require "onyx/sql"` to enable top-level `Onyx.repo` instance and `Onyx.query`,
# `Onyx.exec` and `Onyx.scalar` methods.
class Onyx
end
