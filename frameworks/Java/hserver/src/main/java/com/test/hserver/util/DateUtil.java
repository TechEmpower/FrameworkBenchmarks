package com.test.hserver.util;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Locale;

/**
 * @author hxm
 */
public class DateUtil {
    private static final DateTimeFormatter GMT_FMT = DateTimeFormatter.ofPattern("EEE, dd MMM yyyy HH:mm:ss zzz", Locale.US);
    private static final ZoneId zoneId = ZoneId.of("GMT");
    public static String getNow() {
        return GMT_FMT.format(LocalDateTime.now().atZone(zoneId));
    }
    public static String time;
    public static String getTime(){
        if (time==null){
            time=getNow();
            return time;
        }
        return time;
    }
}
