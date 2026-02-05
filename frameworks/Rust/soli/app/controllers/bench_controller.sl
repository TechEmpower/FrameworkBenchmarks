// TechEmpower Benchmark Controller
// Implements plaintext and json test endpoints per TechEmpower spec

// Plaintext endpoint - returns "Hello, World!" as text/plain
fn plaintext(req: Any) -> Any {
    return {
        "status": 200,
        "headers": {
            "Content-Type": "text/plain",
            "Server": "soli"
        },
        "body": "Hello, World!"
    };
}

// JSON endpoint - returns {"message":"Hello, World!"} as application/json
fn json(req: Any) -> Any {
    return {
        "status": 200,
        "headers": {
            "Content-Type": "application/json",
            "Server": "soli"
        },
        "body": "{\"message\": \"Hello, World!\"}"
    };
}
