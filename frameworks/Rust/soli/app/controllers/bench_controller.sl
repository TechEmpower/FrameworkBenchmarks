// TechEmpower Benchmark Controller
// Implements plaintext and json test endpoints per TechEmpower spec

// Plaintext endpoint - returns "Hello, World!" as text/plain
fn plaintext(req: Any) -> Any {
    return render_text("Hello, World!");
}

// JSON endpoint - returns {"message":"Hello, World!"} as application/json
fn json(req: Any) -> Any {
    return render_json({ "message": "Hello, World!" });
}
