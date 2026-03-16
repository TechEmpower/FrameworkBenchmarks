#ifndef LUNDI_BENCH_JSON_WRITER_HPP
#define LUNDI_BENCH_JSON_WRITER_HPP

#include <lundi/engine/buffer.hpp>
#include <lundi/engine/fast_itoa.hpp>

#include <string_view>
#include <vector>

namespace lundi::bench {

struct world_row;
struct fortune_row;

inline void write_world_json(engine::write_buffer& buf, int id, int random_number) {
    engine::fast_itoa itoa;

    buf.append(R"({"id":)", 6);
    auto* s = itoa.format(static_cast<uint64_t>(id));
    buf.append(s, itoa.length());

    buf.append(R"(,"randomNumber":)", 16);
    s = itoa.format(static_cast<uint64_t>(random_number));
    buf.append(s, itoa.length());

    buf.push('}');
}

inline void write_worlds_json(engine::write_buffer& buf,
                               const std::vector<world_row>& rows) {
    buf.push('[');
    for (size_t i = 0; i < rows.size(); ++i) {
        if (i > 0) buf.push(',');
        write_world_json(buf, rows[i].id, rows[i].random_number);
    }
    buf.push(']');
}

inline void html_escape(engine::write_buffer& buf, std::string_view sv) {
    for (char c : sv) {
        switch (c) {
            case '&':  buf.append("&amp;", 5); break;
            case '<':  buf.append("&lt;", 4); break;
            case '>':  buf.append("&gt;", 4); break;
            case '"':  buf.append("&quot;", 6); break;
            case '\'': buf.append("&#x27;", 6); break;
            default:   buf.push(c); break;
        }
    }
}

inline void write_fortunes_html(engine::write_buffer& buf,
                                 std::vector<fortune_row>& fortunes) {
    // Add the extra fortune
    fortunes.push_back({0, "Additional fortune added at request time."});

    // Sort by message
    std::sort(fortunes.begin(), fortunes.end(),
        [](const fortune_row& a, const fortune_row& b) {
            return a.message < b.message;
        });

    // Write HTML
    static constexpr char HEADER[] =
        "<!DOCTYPE html>"
        "<html><head><title>Fortunes</title></head>"
        "<body><table>"
        "<tr><th>id</th><th>message</th></tr>";
    static constexpr char FOOTER[] =
        "</table></body></html>";

    buf.append(HEADER, sizeof(HEADER) - 1);

    engine::fast_itoa itoa;
    for (const auto& f : fortunes) {
        buf.append("<tr><td>", 8);
        auto* s = itoa.format(static_cast<uint64_t>(f.id));
        buf.append(s, itoa.length());
        buf.append("</td><td>", 9);
        html_escape(buf, f.message);
        buf.append("</td></tr>", 10);
    }

    buf.append(FOOTER, sizeof(FOOTER) - 1);
}

inline void encode_world_response(engine::write_buffer& out, int id, int random_number) {
    // Build body into temporary buffer to get Content-Length
    engine::write_buffer body(128);
    write_world_json(body, id, random_number);

    engine::fast_itoa itoa;
    out.append("HTTP/1.1 200 OK\r\nServer: L\r\nDate: ", 34);
    out.append(engine::global_date_cache().get(), engine::date_cache::DATE_LEN);

    static constexpr char MID[] = "\r\nContent-Type: application/json\r\nContent-Length: ";
    out.append(MID, sizeof(MID) - 1);
    auto* cl = itoa.format(body.size());
    out.append(cl, itoa.length());
    out.append("\r\n\r\n", 4);
    out.append(body.data(), body.size());
}

inline void encode_worlds_response(engine::write_buffer& out,
                                    const std::vector<world_row>& rows) {
    engine::write_buffer body(rows.size() * 40);
    write_worlds_json(body, rows);

    engine::fast_itoa itoa;
    out.append("HTTP/1.1 200 OK\r\nServer: L\r\nDate: ", 34);
    out.append(engine::global_date_cache().get(), engine::date_cache::DATE_LEN);

    static constexpr char MID[] = "\r\nContent-Type: application/json\r\nContent-Length: ";
    out.append(MID, sizeof(MID) - 1);
    auto* cl = itoa.format(body.size());
    out.append(cl, itoa.length());
    out.append("\r\n\r\n", 4);
    out.append(body.data(), body.size());
}

inline void encode_fortunes_response(engine::write_buffer& out,
                                      std::vector<fortune_row>& fortunes) {
    engine::write_buffer body(4096);
    write_fortunes_html(body, fortunes);

    engine::fast_itoa itoa;
    out.append("HTTP/1.1 200 OK\r\nServer: L\r\nDate: ", 34);
    out.append(engine::global_date_cache().get(), engine::date_cache::DATE_LEN);

    static constexpr char MID[] = "\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: ";
    out.append(MID, sizeof(MID) - 1);
    auto* cl = itoa.format(body.size());
    out.append(cl, itoa.length());
    out.append("\r\n\r\n", 4);
    out.append(body.data(), body.size());
}

} // namespace lundi::bench

#endif // LUNDI_BENCH_JSON_WRITER_HPP