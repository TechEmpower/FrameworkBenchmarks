package client

import (
	"github.com/kataras/iris/cache/cfg"
	"github.com/kataras/iris/cache/client/rule"
	"github.com/kataras/iris/cache/ruleset"
	"github.com/kataras/iris/context"
)

// DefaultRuleSet is a list of the default pre-cache validators
// which exists in ALL handlers, local and remote.
var DefaultRuleSet = rule.Chained(
	// #1 A shared cache MUST NOT use a cached response to a request with an
	// Authorization header field
	rule.HeaderClaim(ruleset.AuthorizationRule),
	// #2 "must-revalidate" and/or
	// "s-maxage" response directives are not allowed to be served stale
	// (Section 4.2.4) by shared caches.  In particular, a response with
	// either "max-age=0, must-revalidate" or "s-maxage=0" cannot be used to
	// satisfy a subsequent request without revalidating it on the origin
	// server.
	rule.HeaderClaim(ruleset.MustRevalidateRule),
	rule.HeaderClaim(ruleset.ZeroMaxAgeRule),
	// #3 custom No-Cache header used inside this library
	// for BOTH request and response (after get-cache action)
	rule.Header(ruleset.NoCacheRule, ruleset.NoCacheRule),
)

// NoCache disables the cache for a particular request,
// can be used as a middleware or called manually from the handler.
func NoCache(ctx context.Context) {
	ctx.Header(cfg.NoCacheHeader, "true")
}
