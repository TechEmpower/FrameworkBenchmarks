package cn.ibaijia.tfb;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

/**
 * @author longzl
 */
public class DateUtil {

    private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss z", Locale.ENGLISH);

    private static byte[] date = ("\r\nDate:" + DATE_FORMAT.format(new Date())).getBytes();

    public static byte[] getDate() {
        return date;
    }

    public static void start() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                while (true) {
                    date = ("\r\nDate:" + DATE_FORMAT.format(new Date())).getBytes();
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException e) {

                    }
                }
            }
        }).start();
    }

}
