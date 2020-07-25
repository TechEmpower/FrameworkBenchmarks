from strutils import join
import basolato/middleware


proc corsHeader*(): Headers =
  let allowedMethods = [
    "OPTIONS",
    "GET",
    "POST",
    "PUT",
    "DELETE"
  ]

  let allowedHeaders = [
    "X-login-id",
    "X-login-token"
  ]

  return {
    "Cache-Control": "no-cache",
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Methods": allowedMethods.join(", "),
    "Access-Control-Allow-Headers": allowedHeaders.join(", ")
  }.toHeaders()


proc secureHeader*(): Headers =
  return {
    "Strict-Transport-Security": ["max-age=63072000", "includeSubdomains"].join(", "),
    "X-Frame-Options": "SAMEORIGIN",
    "X-XSS-Protection": ["1", "mode=block"].join(", "),
    "X-Content-Type-Options": "nosniff",
    "Referrer-Policy": ["no-referrer", "strict-origin-when-cross-origin"].join(", "),
    "Cache-control": ["no-cache", "no-store", "must-revalidate"].join(", "),
    "Pragma": "no-cache",
  }.toHeaders()
