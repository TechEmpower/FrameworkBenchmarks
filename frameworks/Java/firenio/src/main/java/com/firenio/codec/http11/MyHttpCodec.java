package com.firenio.codec.http11;

import com.firenio.buffer.ByteBuf;
import com.firenio.collection.ByteTree;
import com.firenio.common.ByteUtil;
import com.firenio.common.Util;
import com.firenio.component.Channel;
import com.firenio.component.FastThreadLocal;
import com.firenio.component.Frame;
import com.firenio.component.NioEventLoop;

import java.io.IOException;

/**
 * @author: wangkai
 **/
public class MyHttpCodec extends HttpCodec {

    public MyHttpCodec(String server, int frameCache, boolean lite, ByteTree cachedUrls) {
        super(server, frameCache, lite, cachedUrls);
    }

    private static String parse_url(ByteBuf src, int url_start, int url_end) {
        StringBuilder line = FastThreadLocal.get().getStringBuilder();
        for (int i = url_start; i < url_end; i++) {
            line.append((char) (src.getByteAbs(i) & 0xff));
        }
        return line.toString();
    }

    private static int read_line(StringBuilder line, ByteBuf src, int abs_pos, int length, int limit) throws IOException {
        int maybeRead = limit - length;
        int s_limit   = src.absWriteIndex();
        int remaining = s_limit - abs_pos;
        if (remaining > maybeRead) {
            int i = read_line(line, src, abs_pos, abs_pos + maybeRead);
            if (i == -1) {
                throw OVER_LIMIT;
            }
            return i;
        } else {
            return read_line(line, src, abs_pos, s_limit);
        }
    }

    private static int read_line(StringBuilder line, ByteBuf src, int abs_pos, int abs_limit) {
        for (int i = abs_pos; i < abs_limit; i++) {
            byte b = src.getByteAbs(i);
            if (b == N) {
                line.setLength(line.length() - 1);
                return i + 1;
            } else {
                line.append((char) (b & 0xff));
            }
        }
        return -1;
    }

    private static int read_line_range(ByteBuf src, int abs_pos, int h_len, int limit) throws IOException {
        int to          = abs_pos + (limit - h_len);
        int write_index = src.writeIndex();
        if (to > write_index) {
            return src.indexOf(N, abs_pos, write_index);
        } else {
            int index = src.indexOf(N, abs_pos, to);
            if (index == -1) {
                throw OVER_LIMIT;
            }
            return index;
        }
    }

    private static boolean start_with(ByteBuf src, int ps, int pe, byte[] match) {
        if (pe - ps < match.length) {
            return false;
        }
        for (int i = 0; i < match.length; i++) {
            if (src.getByteAbs(ps + i) != match[i]) {
                return false;
            }
        }
        return true;
    }

    private HttpFrame alloc_frame(NioEventLoop el) {
        if (fcache > 0) {
            Frame res = (Frame) el.getCache(FRAME_CACHE_KEY, fcache);
            if (res == null) {
                return new_frame();
            } else {
                return (HttpFrame) res.reset();
            }
        }
        return new_frame();
    }

    private int decode_lite(ByteBuf src, HttpFrame f) throws IOException {
        int decode_state = f.getDecodeState();
        int abs_pos      = src.absReadIndex();
        int h_len        = f.getHeaderLength();
        if (decode_state == decode_state_line_one) {
            int l_end = read_line_range(src, abs_pos, h_len, hlimit);
            if (l_end == -1) {
                return decode_state_line_one;
            } else {
                h_len += (l_end - abs_pos);
                decode_state = decode_state_header;
                int url_start = abs_pos;
                int num       = src.getIntLE(abs_pos);
                if (num == NUM_GET) {
                    f.setMethod(HttpMethod.GET);
                    url_start += 4;
                } else if (num == NUM_POST) {
                    f.setMethod(HttpMethod.POST);
                    url_start += 5;
                } else if (num == NUM_HEAD) {
                    f.setMethod(HttpMethod.HEAD);
                    url_start += 5;
                } else {
                    throw ILLEGAL_METHOD;
                }
                int url_end = l_end - 10;
                int qmark   = src.indexOf((byte) '?', url_start, url_end);
                if (qmark == -1) {
                    String url;
                    if (cached_urls != null) {
                        url = cached_urls.getString(src, url_start, url_end);
                        if (url == null) {
                            url = parse_url(src, url_start, url_end);
                        }
                    } else {
                        url = parse_url(src, url_start, url_end);
                    }
                    f.setRequestURL(url);
                } else {
                    StringBuilder line = FastThreadLocal.get().getStringBuilder();
                    for (int i = url_start; i < url_end; i++) {
                        line.append((char) (src.getByteAbs(i) & 0xff));
                    }
                    int re_qmark = qmark - url_start;
                    parse_kv(f.getRequestParams(), line, re_qmark + 1, line.length(), '=', '&');
                    f.setRequestURL((String) line.subSequence(0, re_qmark));
                }
                abs_pos = l_end + 1;
            }
        }
        if (decode_state == decode_state_header) {
            for (; ; ) {
                int ps = abs_pos;
                int pe = read_line_range(src, ps, h_len, hlimit);
                if (pe == -1) {
                    f.setHeaderLength(h_len);
                    src.absReadIndex(abs_pos);
                    break;
                }
                abs_pos = pe-- + 1;
                int size = pe - ps;
                h_len += size;
                if (size == 0) {
                    if (f.getContentLength() < 1) {
                        decode_state = decode_state_complete;
                    } else {
                        if (f.getContentLength() > blimit) {
                            throw OVER_LIMIT;
                        }
                        decode_state = decode_state_body;
                    }
                    src.absReadIndex(abs_pos);
                    break;
                } else {
                    if (!f.isGet()) {
                        if (start_with(src, ps, pe, CONTENT_LENGTH_MATCH)) {
                            int cp  = ps + CONTENT_LENGTH_MATCH.length;
                            int cps = ByteUtil.skip(src, cp, pe, SPACE);
                            if (cps == -1) {
                                throw OVER_LIMIT;
                            }
                            int ct_len = 0;
                            for (int i = cps; i < pe; i++) {
                                ct_len = (src.getByteAbs(i) - '0') + ct_len * 10;
                            }
                            f.setContentLength(ct_len);
                        }
                    }
                }
            }
        }
        return decode_state;
    }

    private int decode_full(ByteBuf src, HttpFrame f) throws IOException {
        StringBuilder line         = FastThreadLocal.get().getStringBuilder();
        int           decode_state = f.getDecodeState();
        int           h_len        = f.getHeaderLength();
        int           abs_pos      = src.absReadIndex();
        if (decode_state == decode_state_line_one) {
            int l_end = read_line(line, src, abs_pos, 0, hlimit);
            if (l_end == -1) {
                return decode_state_line_one;
            } else {
                abs_pos = l_end;
                h_len += line.length();
                decode_state = decode_state_header;
                parse_line_one(f, line);
            }
        }
        if (decode_state == decode_state_header) {
            for (; ; ) {
                line.setLength(0);
                int pn = read_line(line, src, abs_pos, h_len, hlimit);
                if (pn == -1) {
                    src.absReadIndex(abs_pos);
                    f.setHeaderLength(h_len);
                    break;
                }
                abs_pos = pn;
                h_len += line.length();
                if (line.length() == 0) {
                    src.absReadIndex(abs_pos);
                    decode_state = header_complete(f);
                    break;
                } else {
                    int p = Util.indexOf(line, ':');
                    if (p == -1) {
                        continue;
                    }
                    int    rp    = Util.skip(line, ' ', p + 1);
                    String name  = line.substring(0, p);
                    String value = line.substring(rp);
                    f.setReadHeader(name, value);
                }
            }
        }
        return decode_state;
    }

    @Override
    public Frame decode(Channel ch, ByteBuf src) throws Exception {
        boolean        remove = false;
        HttpAttachment att    = (HttpAttachment) ch.getAttachment();
        HttpFrame      f      = att.getUncompletedFrame();
        if (f == null) {
            f = alloc_frame(ch.getEventLoop());
        } else {
            remove = true;
        }
        int decode_state;
        if (lite) {
            decode_state = decode_lite(src, f);
        } else {
            decode_state = decode_full(src, f);
        }
        if (decode_state == decode_state_body) {
            decode_state = decode_remain_body(ch, src, f);
        }
        if (decode_state == decode_state_complete) {
            if (remove) {
                att.setUncompletedFrame(null);
            }
            return f;
        } else {
            f.setDecodeState(decode_state);
            att.setUncompletedFrame(f);
            return null;
        }
    }

}
