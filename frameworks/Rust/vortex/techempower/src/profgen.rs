//! PGO profiling harness for Vortex hot paths.
//!
//! Exercises HTTP parsing, response generation, JSON serialization,
//! and template rendering to generate LLVM PGO profile data.
//! Does NOT require io_uring or network access — safe to run during
//! `docker build`.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::hint::black_box;
use vortex_http::date::DateCache;
use vortex_http::parser;
use vortex_http::pipeline;
use vortex_http::response::{DynHtmlResponse, DynJsonResponse};

fn main() {
    let mut date = DateCache::new();

    // Realistic HTTP requests matching TFB wrk format
    let plaintext_req =
        b"GET /plaintext HTTP/1.1\r\nHost: tfb-server:8080\r\nAccept: */*\r\n\r\n";
    let json_req =
        b"GET /json HTTP/1.1\r\nHost: tfb-server:8080\r\nAccept: */*\r\n\r\n";
    let queries_req =
        b"GET /queries?q=20 HTTP/1.1\r\nHost: tfb-server:8080\r\nAccept: */*\r\n\r\n";
    let updates_req =
        b"GET /updates?q=20 HTTP/1.1\r\nHost: tfb-server:8080\r\nAccept: */*\r\n\r\n";
    let fortunes_req =
        b"GET /fortunes HTTP/1.1\r\nHost: tfb-server:8080\r\nAccept: */*\r\n\r\n";
    let db_req =
        b"GET /db HTTP/1.1\r\nHost: tfb-server:8080\r\nAccept: */*\r\n\r\n";

    // 16x pipelined plaintext (TechEmpower methodology)
    let mut pipelined = Vec::new();
    for _ in 0..16 {
        pipelined.extend_from_slice(plaintext_req);
    }

    let mut send_buf = vec![0u8; 65536];
    let mut body_buf = vec![0u8; 32768];
    let mut html_buf = Vec::with_capacity(4096);

    // Realistic fortune data (matches TFB schema)
    let fortunes: Vec<(i32, String)> = vec![
        (1, "fortune: No such file or directory".into()),
        (2, "A computer scientist is someone who fixes things that aren't broken.".into()),
        (3, "After enough decimal places, nobody gives a damn.".into()),
        (4, "A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1".into()),
        (5, "A computer program does what you tell it to do, not what you want it to do.".into()),
        (6, "Emacs is a nice operating system, but I prefer UNIX. \u{2014} Tom Christensen".into()),
        (7, "Any program that runs right is obsolete.".into()),
        (8, "A list is only as strong as its weakest link. \u{2014} Donald Knuth".into()),
        (9, "Feature: A bug with seniority.".into()),
        (10, "Computers make very fast, very accurate mistakes.".into()),
        (11, "<script>alert(\"This should not be displayed in a browser alert box.\");</script>".into()),
        (12, "\u{30D5}\u{30EC}\u{30FC}\u{30E0}\u{30EF}\u{30FC}\u{30AF}\u{306E}\u{30D9}\u{30F3}\u{30C1}\u{30DE}\u{30FC}\u{30AF}".into()),
    ];

    // Exercise hot paths proportional to real TFB workload:
    // - Plaintext pipelined: highest volume
    // - JSON: high volume
    // - DB/queries/updates JSON: medium volume
    // - Fortunes HTML: lower volume
    for i in 0..500_000u32 {
        date.maybe_update();

        // Route classification (runs on every recv)
        black_box(parser::classify_fast(plaintext_req));
        black_box(parser::classify_fast(json_req));
        black_box(parser::classify_fast(db_req));
        black_box(parser::classify_fast(queries_req));
        black_box(parser::classify_fast(fortunes_req));
        black_box(parser::classify_fast(updates_req));

        // Query parameter parsing
        black_box(parser::parse_queries_param(queries_req));
        black_box(parser::parse_queries_param(updates_req));

        // Request boundary detection
        black_box(parser::find_request_end(plaintext_req));
        black_box(parser::count_request_boundaries(&pipelined));

        // Pipelined response generation (primary plaintext hot path)
        black_box(pipeline::process_pipelined(
            &pipelined,
            &mut send_buf,
            &date,
        ));

        // Single JSON response
        black_box(pipeline::process_pipelined(
            json_req.as_slice(),
            &mut send_buf,
            &date,
        ));

        // Dynamic JSON: single world (/db endpoint)
        let id = (i % 10000 + 1) as i32;
        let rn = (i.wrapping_mul(7).wrapping_add(3) % 10000 + 1) as i32;
        let wlen = vortex_json::write_world(&mut body_buf, id, rn);
        black_box(DynJsonResponse::write(
            &mut send_buf,
            &date,
            &body_buf[..wlen],
        ));

        // Multi-world JSON: 20 worlds (/queries, /updates endpoints)
        if i % 10 == 0 {
            let worlds: Vec<(i32, i32)> = (0..20)
                .map(|j| {
                    (
                        (i as i32 + j) % 10000 + 1,
                        (i as i32 * 7 + j) % 10000 + 1,
                    )
                })
                .collect();
            let wlen = vortex_json::write_worlds(&mut body_buf, &worlds);
            black_box(DynJsonResponse::write(
                &mut send_buf,
                &date,
                &body_buf[..wlen],
            ));
        }

        // Fortunes template rendering (/fortunes endpoint)
        if i % 50 == 0 {
            vortex_template::render_fortunes(&fortunes, &mut html_buf);
            black_box(DynHtmlResponse::write(
                &mut send_buf,
                &date,
                &html_buf,
            ));
        }
    }

    eprintln!("[profgen] PGO profile data generated");
}
