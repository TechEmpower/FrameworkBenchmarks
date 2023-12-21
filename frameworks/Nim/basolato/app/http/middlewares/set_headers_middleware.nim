import std/asyncdispatch
import std/httpcore
import basolato/middleware


proc setCorsHeaders*(c:Context, p:Params):Future[Response] {.async.} =
  let allowedMethods = [
    "OPTIONS",
    "GET",
    "POST",
    "PUT",
    "DELETE"
  ]

  let allowedHeaders = [
    "Access-Control-Allow-Origin",
    "Content-Type",
    "*",
  ]

  let headers = {
    "Origin": @[$true],
    "Cache-Control": @["no-cache"],
    "Access-Control-Allow-Credentials": @[$true],
    "Access-Control-Allow-Origin": @["http://localhost:3000"],
    "Access-Control-Allow-Methods": @allowedMethods,
    "Access-Control-Allow-Headers": @allowedHeaders,
    "Access-Control-Expose-Headers": @allowedHeaders,
  }.newHttpHeaders()
  return next(status=Http204, headers=headers)


proc setSecureHeaders*(c:Context, p:Params):Future[Response] {.async.} =
  let headers = {
    "Strict-Transport-Security": @["max-age=63072000", "includeSubdomains"],
    "X-Frame-Options": @["SAMEORIGIN"],
    "X-XSS-Protection": @["1", "mode=block"],
    "X-Content-Type-Options": @["nosniff"],
    "Referrer-Policy": @["no-referrer", "strict-origin-when-cross-origin"],
    "Cache-Control": @["no-cache", "no-store", "must-revalidate"],
    "Pragma": @["no-cache"],
  }.newHttpHeaders()
  return next(status=Http204, headers=headers)
