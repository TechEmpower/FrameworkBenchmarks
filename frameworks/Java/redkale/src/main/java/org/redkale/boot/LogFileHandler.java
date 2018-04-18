/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.boot;

import java.io.*;
import java.nio.file.*;
import static java.nio.file.StandardCopyOption.*;
import java.time.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import java.util.logging.*;
import java.util.logging.Formatter;
import java.util.regex.Pattern;

/**
 * 自定义的日志输出类
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@SuppressWarnings("unchecked")
public class LogFileHandler extends Handler {

    /**
     * SNCP的日志输出Handler
     */
    public static class SncpLogFileHandler extends LogFileHandler {

        @Override
        public String getPrefix() {
            return "sncp-";
        }
    }

    /**
     * 默认的日志时间格式化类
     *
     */
    public static class LoggingFormater extends Formatter {

        private static final String format = "%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS.%tL %4$s %2$s\r\n%5$s%6$s\r\n";

        @Override
        public String format(LogRecord record) {
            String source;
            if (record.getSourceClassName() != null) {
                source = record.getSourceClassName();
                if (record.getSourceMethodName() != null) {
                    source += " " + record.getSourceMethodName();
                }
            } else {
                source = record.getLoggerName();
            }
            String message = formatMessage(record);
            String throwable = "";
            if (record.getThrown() != null) {
                StringWriter sw = new StringWriter();
                PrintWriter pw = new PrintWriter(sw) {
                    @Override
                    public void println() {
                        super.print("\r\n");
                    }
                };
                pw.println();
                record.getThrown().printStackTrace(pw);
                pw.close();
                throwable = sw.toString();
            }
            return String.format(format,
                System.currentTimeMillis(),
                source,
                record.getLoggerName(),
                record.getLevel().getName(),
                message,
                throwable);
        }

    }

    protected final LinkedBlockingQueue<LogRecord> records = new LinkedBlockingQueue();

    private String pattern;

    private String unusual; //不为null表示将 WARNING、SEVERE 级别的日志写入单独的文件中

    private int limit;   //文件大小限制

    private final AtomicInteger logindex = new AtomicInteger();

    private final AtomicInteger logunusualindex = new AtomicInteger();

    private int count = 1;  //文件限制

    private long tomorrow;

    private boolean append;

    private Pattern denyreg;

    private final AtomicLong loglength = new AtomicLong();

    private final AtomicLong logunusuallength = new AtomicLong();

    private File logfile;

    private File logunusualfile;

    private OutputStream logstream;

    private OutputStream logunusualstream;

    public LogFileHandler() {
        updateTomorrow();
        configure();
        open();
    }

    private void updateTomorrow() {
        Calendar cal = Calendar.getInstance();
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        cal.add(Calendar.DAY_OF_YEAR, 1);
        long t = cal.getTimeInMillis();
        if (this.tomorrow != t) logindex.set(0);
        this.tomorrow = t;
    }

    private void open() {
        final String name = "Logging-" + getClass().getSimpleName() + "-Thread";
        new Thread() {
            {
                setName(name);
                setDaemon(true);
            }

            @Override
            public void run() {
                while (true) {
                    try {
                        LogRecord record = records.take();
                        final boolean bigger = (limit > 0 && limit <= loglength.get());
                        final boolean changeday = tomorrow <= record.getMillis();
                        if (bigger || changeday) {
                            updateTomorrow();
                            if (logstream != null) {
                                logstream.close();
                                if (bigger) {
                                    for (int i = Math.min(count - 2, logindex.get() - 1); i > 0; i--) {
                                        File greater = new File(logfile.getPath() + "." + i);
                                        if (greater.exists()) Files.move(greater.toPath(), new File(logfile.getPath() + "." + (i + 1)).toPath(), REPLACE_EXISTING, ATOMIC_MOVE);
                                    }
                                    Files.move(logfile.toPath(), new File(logfile.getPath() + ".1").toPath(), REPLACE_EXISTING, ATOMIC_MOVE);
                                } else {
                                    if (logfile.exists() && logfile.length() < 1) logfile.delete();
                                }
                                logstream = null;
                            }
                        }
                        if (unusual != null && changeday && logunusualstream != null) {
                            logunusualstream.close();
                            if (limit > 0 && limit <= logunusuallength.get()) {
                                for (int i = Math.min(count - 2, logunusualindex.get() - 1); i > 0; i--) {
                                    File greater = new File(logunusualfile.getPath() + "." + i);
                                    if (greater.exists()) Files.move(greater.toPath(), new File(logunusualfile.getPath() + "." + (i + 1)).toPath(), REPLACE_EXISTING, ATOMIC_MOVE);
                                }
                                Files.move(logunusualfile.toPath(), new File(logunusualfile.getPath() + ".1").toPath(), REPLACE_EXISTING, ATOMIC_MOVE);
                            } else {
                                if (logunusualfile.exists() && logunusualfile.length() < 1) logunusualfile.delete();
                            }
                            logunusualstream = null;
                        }
                        if (logstream == null) {
                            logindex.incrementAndGet();
                            java.time.LocalDate date = LocalDate.now();
                            logfile = new File(pattern.replace("%m", String.valueOf((date.getYear() * 100 + date.getMonthValue()))).replace("%d", String.valueOf((date.getYear() * 10000 + date.getMonthValue() * 100 + date.getDayOfMonth()))));
                            logfile.getParentFile().mkdirs();
                            loglength.set(logfile.length());
                            logstream = new FileOutputStream(logfile, append);
                        }
                        if (unusual != null && logunusualstream == null) {
                            logunusualindex.incrementAndGet();
                            java.time.LocalDate date = LocalDate.now();
                            logunusualfile = new File(unusual.replace("%m", String.valueOf((date.getYear() * 100 + date.getMonthValue()))).replace("%d", String.valueOf((date.getYear() * 10000 + date.getMonthValue() * 100 + date.getDayOfMonth()))));
                            logunusualfile.getParentFile().mkdirs();
                            logunusuallength.set(logunusualfile.length());
                            logunusualstream = new FileOutputStream(logunusualfile, append);
                        }
                        //----------------------写日志-------------------------
                        String message = getFormatter().format(record);
                        String encoding = getEncoding();
                        byte[] bytes = encoding == null ? message.getBytes() : message.getBytes(encoding);
                        logstream.write(bytes);
                        loglength.addAndGet(bytes.length);
                        if (unusual != null && (record.getLevel() == Level.WARNING || record.getLevel() == Level.SEVERE)) {
                            logunusualstream.write(bytes);
                            logunusuallength.addAndGet(bytes.length);
                        }
                    } catch (Exception e) {
                        ErrorManager err = getErrorManager();
                        if (err != null) err.error(null, e, ErrorManager.WRITE_FAILURE);
                    }
                }

            }
        }.start();
    }

    public String getPrefix() {
        return "";
    }

    private void configure() {
        LogManager manager = LogManager.getLogManager();
        String cname = LogFileHandler.class.getName();
        this.pattern = manager.getProperty(cname + ".pattern");
        if (this.pattern == null) {
            this.pattern = "logs-%m/" + getPrefix() + "log-%d.log";
        } else {
            int pos = this.pattern.lastIndexOf('/');
            if (pos > 0) {
                this.pattern = this.pattern.substring(0, pos + 1) + getPrefix() + this.pattern.substring(pos + 1);
            } else {
                this.pattern = getPrefix() + this.pattern;
            }
        }
        String unusualstr = manager.getProperty(cname + ".unusual");
        if (unusualstr != null) {
            int pos = unusualstr.lastIndexOf('/');
            if (pos > 0) {
                this.unusual = unusualstr.substring(0, pos + 1) + getPrefix() + unusualstr.substring(pos + 1);
            } else {
                this.unusual = getPrefix() + unusualstr;
            }
        }
        String limitstr = manager.getProperty(cname + ".limit");
        try {
            if (limitstr != null) this.limit = Math.abs(Integer.decode(limitstr));
        } catch (Exception e) {
        }
        String countstr = manager.getProperty(cname + ".count");
        try {
            if (countstr != null) this.count = Math.max(1, Math.abs(Integer.decode(countstr)));
        } catch (Exception e) {
        }
        String appendstr = manager.getProperty(cname + ".append");
        try {
            if (appendstr != null) this.append = "true".equalsIgnoreCase(appendstr) || "1".equals(appendstr);
        } catch (Exception e) {
        }
        String levelstr = manager.getProperty(cname + ".level");
        try {
            if (levelstr != null) {
                Level l = Level.parse(levelstr);
                setLevel(l != null ? l : Level.ALL);
            }
        } catch (Exception e) {
        }
        String filterstr = manager.getProperty(cname + ".filter");
        try {
            if (filterstr != null) {
                Class<?> clz = ClassLoader.getSystemClassLoader().loadClass(filterstr);
                setFilter((Filter) clz.getDeclaredConstructor().newInstance());
            }
        } catch (Exception e) {
        }
        String formatterstr = manager.getProperty(cname + ".formatter");
        try {
            if (formatterstr != null) {
                Class<?> clz = ClassLoader.getSystemClassLoader().loadClass(formatterstr);
                setFormatter((Formatter) clz.getDeclaredConstructor().newInstance());
            }
        } catch (Exception e) {
        }
        if (getFormatter() == null) setFormatter(new SimpleFormatter());

        String encodingstr = manager.getProperty(cname + ".encoding");
        try {
            if (encodingstr != null) setEncoding(encodingstr);
        } catch (Exception e) {
        }

        String denyregstr = manager.getProperty(cname + ".denyreg");
        try {
            if (denyregstr != null && !denyregstr.trim().isEmpty()) {
                denyreg = Pattern.compile(denyregstr);
            }
        } catch (Exception e) {
        }
    }

    @Override
    public void publish(LogRecord record) {
        final String sourceClassName = record.getSourceClassName();
        if (sourceClassName == null || true) {
            StackTraceElement[] ses = new Throwable().getStackTrace();
            for (int i = 2; i < ses.length; i++) {
                if (ses[i].getClassName().startsWith("java.util.logging")) continue;
                record.setSourceClassName('[' + Thread.currentThread().getName() + "] " + ses[i].getClassName());
                record.setSourceMethodName(ses[i].getMethodName());
                break;
            }
        } else {
            record.setSourceClassName('[' + Thread.currentThread().getName() + "] " + sourceClassName);
        }
        if (denyreg != null && denyreg.matcher(record.getMessage()).find()) return;
        records.offer(record);
    }

    @Override
    public void flush() {
        try {
            if (logstream != null) logstream.flush();
        } catch (Exception e) {
            ErrorManager err = getErrorManager();
            if (err != null) err.error(null, e, ErrorManager.FLUSH_FAILURE);
        }
    }

    @Override
    public void close() throws SecurityException {
        try {
            if (logstream != null) logstream.close();
        } catch (Exception e) {
            ErrorManager err = getErrorManager();
            if (err != null) err.error(null, e, ErrorManager.CLOSE_FAILURE);
        }
    }

}
