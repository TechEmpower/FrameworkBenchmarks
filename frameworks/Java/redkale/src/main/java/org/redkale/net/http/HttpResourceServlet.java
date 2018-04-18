/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import java.io.*;
import java.nio.ByteBuffer;
import static java.nio.file.StandardWatchEventKinds.*;
import java.nio.file.*;
import java.util.AbstractMap.SimpleEntry;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.LongAdder;
import java.util.logging.*;
import java.util.regex.*;
import org.redkale.util.AnyValue;

/**
 * 静态资源HttpServlet
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class HttpResourceServlet extends HttpServlet {

    protected final Logger logger = Logger.getLogger(this.getClass().getSimpleName());

    protected class WatchThread extends Thread {

        protected final File root;

        protected final WatchService watcher;

        public WatchThread(File root) throws IOException {
            this.root = root;
            this.setName("HttpResourceServlet-Watch-Thread");
            this.setDaemon(true);
            this.watcher = this.root.toPath().getFileSystem().newWatchService();
        }

        @Override
        public void run() {
            try {
                final String rootstr = root.getCanonicalPath();
                while (!this.isInterrupted()) {
                    final WatchKey key = watcher.take();
                    final Path parent = keymaps.get(key);
                    if (parent == null) {
                        key.cancel();
                        continue;
                    }
                    key.pollEvents().stream().forEach((event) -> {
                        try {
                            Path path = parent.resolve((Path) event.context());
                            final String uri = path.toString().substring(rootstr.length()).replace('\\', '/');
                            //logger.log(Level.FINEST, "file(" + uri + ") happen " + event.kind() + " event");
                            if (event.kind() == ENTRY_DELETE) {
                                FileEntry en = files.remove(uri);
                                if (en != null) en.remove();
                            } else if (event.kind() == ENTRY_MODIFY) {
                                FileEntry en = files.get(uri);
                                if (en != null && en.file != null) {
                                    long d;  //等待update file完毕
                                    for (;;) {
                                        d = en.file.lastModified();
                                        Thread.sleep(2000L);
                                        if (d == en.file.lastModified()) break;
                                    }
                                    en.update();
                                }
                            }
                        } catch (Exception ex) {
                            logger.log(Level.FINE, event.context() + " occur erroneous", ex);
                        }
                    });
                    key.reset();
                }
            } catch (Exception e) {
            }
        }
    }

    protected final LongAdder cachedLength = new LongAdder();

    //缓存总大小, 默认0
    protected long cachelimit = 0 * 1024 * 1024L;

    //最大可缓存的文件大小，  大于该值的文件将不被缓存
    protected long cachelengthmax = 1 * 1024 * 1024;

    //是否监控缓存文件的变化， 默认不监控
    protected boolean watch = false;

    protected File root = new File("./root/");

    protected String indexHtml = "index.html";

    protected final ConcurrentHashMap<String, FileEntry> files = new ConcurrentHashMap<>();

    protected final ConcurrentHashMap<WatchKey, Path> keymaps = new ConcurrentHashMap<>();

    protected SimpleEntry<Pattern, String>[] locationRewrites;

    protected WatchThread watchThread;

    @Override
    public void init(HttpContext context, AnyValue config) {
        if (config != null) {
            String rootstr = config.getValue("webroot", "root");
            this.indexHtml = config.getValue("index", "index.html");
            if (rootstr.indexOf(':') < 0 && rootstr.indexOf('/') != 0 && System.getProperty("APP_HOME") != null) {
                rootstr = new File(System.getProperty("APP_HOME"), rootstr).getPath();
            }
            try {
                this.root = new File(rootstr).getCanonicalFile();
            } catch (IOException ioe) {
                this.root = new File(rootstr);
            }
            AnyValue cacheconf = config.getAnyValue("cache");
            if (cacheconf != null) {
                this.cachelimit = parseLenth(cacheconf.getValue("limit"), 0 * 1024 * 1024L);
                this.cachelengthmax = parseLenth(cacheconf.getValue("lengthmax"), 1 * 1024 * 1024L);
                this.watch = cacheconf.getBoolValue("watch", false);
            }
            List<SimpleEntry<Pattern, String>> locations = new ArrayList<>();
            for (AnyValue av : config.getAnyValues("rewrite")) {
                if ("location".equals(av.getValue("type"))) {
                    String m = av.getValue("match");
                    String f = av.getValue("forward");
                    if (m != null && f != null) {
                        locations.add(new SimpleEntry<>(Pattern.compile(m), f));
                    }
                }
            }
            this.locationRewrites = locations.isEmpty() ? null : locations.toArray(new SimpleEntry[locations.size()]);
        }
        if (this.cachelimit < 1) return;  //不缓存不需要开启WatchThread监听
        if (this.root != null && this.watch) {
            try {
                this.watchThread = new WatchThread(this.root);
                this.watchThread.start();
            } catch (IOException ex) {
                logger.log(Level.WARNING, HttpResourceServlet.class.getSimpleName() + " start watch-thread error", ex);
            }
        }
    }

    @Override
    public void destroy(HttpContext context, AnyValue config) {
        if (this.watchThread != null) {
            try {
                this.watchThread.watcher.close();
            } catch (IOException ex) {
                logger.log(Level.WARNING, HttpResourceServlet.class.getSimpleName() + " close watch-thread error", ex);
            }
            if (this.watchThread.isAlive()) this.watchThread.interrupt();
        }
    }

    public void serRoot(String rootstr) {
        if (rootstr == null) return;
        try {
            this.root = new File(rootstr).getCanonicalFile();
        } catch (IOException ioe) {
            this.root = new File(rootstr);
        }
    }

    public void serRoot(File file) {
        if (file == null) return;
        try {
            this.root = file.getCanonicalFile();
        } catch (IOException ioe) {
            this.root = file;
        }
    }

    protected static long parseLenth(String value, long defValue) {
        if (value == null) return defValue;
        value = value.toUpperCase().replace("B", "");
        if (value.endsWith("G")) return Long.decode(value.replace("G", "")) * 1024 * 1024 * 1024;
        if (value.endsWith("M")) return Long.decode(value.replace("M", "")) * 1024 * 1024;
        if (value.endsWith("K")) return Long.decode(value.replace("K", "")) * 1024;
        return Long.decode(value);
    }

    @Override
    public void execute(HttpRequest request, HttpResponse response) throws IOException {
        String uri = request.getRequestURI();
        if (uri.contains("../")) {
            if (logger.isLoggable(Level.FINEST)) logger.log(Level.FINEST, "Not found resource (404) be " + uri + ", request = " + request);
            response.finish404();
            return;
        }
        if (locationRewrites != null) {
            for (SimpleEntry<Pattern, String> entry : locationRewrites) {
                Matcher matcher = entry.getKey().matcher(uri);
                if (matcher.find()) {
                    StringBuffer sb = new StringBuffer(uri.length());
                    matcher.appendReplacement(sb, entry.getValue());
                    matcher.appendTail(sb);
                    uri = sb.toString();
                    break;
                }
            }
        }
        if (uri.length() == 0 || uri.equals("/")) {
            uri = this.indexHtml.indexOf('/') == 0 ? this.indexHtml : ("/" + this.indexHtml);
        }
        //System.out.println(request);
        FileEntry entry;
        if (watchThread == null && files.isEmpty()) {
            entry = createFileEntry(uri);
        } else {  //有缓存
            entry = files.computeIfAbsent(uri, x -> createFileEntry(x));
        }
        if (entry == null) {
            if (logger.isLoggable(Level.FINER)) logger.log(Level.FINER, "Not found resource (404), request = " + request);
            response.finish404();
        } else {
            //file = null 表示资源内容在内存而不是在File中
            //file = null 时必须传 filename
            response.finishFile(entry.file == null ? entry.filename : null, entry.file, entry.content);
        }
    }

    protected FileEntry createFileEntry(String uri) {
        File rfile = new File(root, uri);
        File file = rfile;
        if (file.isDirectory()) file = new File(rfile, this.indexHtml);
        if (file.isDirectory()) file = new File(rfile, "index.html");
        if (!file.isFile() || !file.canRead()) return null;
        FileEntry en = new FileEntry(this, file);
        if (watchThread == null) return en;
        try {
            Path p = file.getParentFile().toPath();
            keymaps.put(p.register(watchThread.watcher, ENTRY_MODIFY, ENTRY_DELETE), p);
        } catch (IOException e) {
            logger.log(Level.INFO, HttpResourceServlet.class.getSimpleName() + " watch FileEntry(" + uri + ") erroneous", e);
        }
        return en;
    }

    protected static class FileEntry {

        protected final String filename;

        protected final File file; //如果所有资源文件打包成zip文件则file=null

        protected final HttpResourceServlet servlet;

        protected ByteBuffer content;

        public FileEntry(final HttpResourceServlet servlet, File file) {
            this.servlet = servlet;
            this.file = file;
            this.filename = file.getName();
            update();
        }

        public FileEntry(final HttpResourceServlet servlet, String filename, ByteBuffer content) {
            this.servlet = servlet;
            this.file = null;
            this.filename = filename;
            this.content = content.asReadOnlyBuffer();
            this.servlet.cachedLength.add(this.content.remaining());
        }

        public FileEntry(final HttpResourceServlet servlet, String filename, InputStream in) throws IOException {
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            byte[] bytes = new byte[10240];
            int pos;
            while ((pos = in.read(bytes)) != -1) {
                out.write(bytes, 0, pos);
            }
            byte[] bs = out.toByteArray();
            ByteBuffer buf = ByteBuffer.allocateDirect(bs.length);
            buf.put(bs);
            buf.flip();

            this.servlet = servlet;
            this.file = null;
            this.filename = filename;
            this.content = buf.asReadOnlyBuffer();
            this.servlet.cachedLength.add(this.content.remaining());
        }

        public void update() {
            if (this.file == null) return;
            if (this.content != null) {
                this.servlet.cachedLength.add(0L - this.content.remaining());
                this.content = null;
            }
            long length = this.file.length();
            if (length > this.servlet.cachelengthmax) return;
            if (this.servlet.cachedLength.longValue() + length > this.servlet.cachelimit) return; //超过缓存总容量
            try {
                FileInputStream in = new FileInputStream(file);
                ByteArrayOutputStream out = new ByteArrayOutputStream((int) file.length());
                byte[] bytes = new byte[10240];
                int pos;
                while ((pos = in.read(bytes)) != -1) {
                    out.write(bytes, 0, pos);
                }
                in.close();
                byte[] bs = out.toByteArray();
                ByteBuffer buf = ByteBuffer.allocateDirect(bs.length);
                buf.put(bs);
                buf.flip();
                this.content = buf.asReadOnlyBuffer();
                this.servlet.cachedLength.add(this.content.remaining());
            } catch (Exception e) {
                this.servlet.logger.log(Level.INFO, HttpResourceServlet.class.getSimpleName() + " update FileEntry(" + file + ") erroneous", e);
            }
        }

        public void remove() {
            if (this.content != null) this.servlet.cachedLength.add(0L - this.content.remaining());
        }

        public long getCachedLength() {
            return this.content == null ? 0L : this.content.remaining();
        }

    }
}
