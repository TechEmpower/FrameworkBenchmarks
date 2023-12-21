import std/asyncdispatch
import std/options
import std/times
import basolato/core/baseEnv
import basolato/middleware
import basolato/core/security/cookie
import basolato/core/security/session


proc sessionFromCookie*(c:Context, p:Params):Future[Response] {.async.} =
  let sessionId = Cookies.new(c.request).get("session_id")
  let sessionOpt = Session.new(sessionId).await
  sessionOpt.updateNonce().await
  c.setSession(sessionOpt.get())
  let newSessionId = sessionOpt.getToken().await
  var cookies = Cookies.new(c.request)
  cookies.set("session_id", newSessionId, expire=timeForward(SESSION_TIME, Minutes))
  return next().setCookie(cookies)
