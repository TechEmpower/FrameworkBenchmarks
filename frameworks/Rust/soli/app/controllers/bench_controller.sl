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

// Fortune endpoint - queries fortunes from SoliDB, adds runtime fortune, sorts, renders HTML
fn fortunes(req: Any) -> Any {
    let fortunes = Fortune.all();
    fortunes.push({ "id": 0, "message": "Additional fortune added at request time." });
    let sorted = fortunes.sort_by("message");
    return render("fortunes/index", { "fortunes": sorted });
}
