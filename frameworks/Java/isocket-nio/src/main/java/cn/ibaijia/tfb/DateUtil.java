package cn.ibaijia.tfb;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

public class DateUtil {

    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss z", Locale.ENGLISH);//Fri, 09 Jul 2021 09:10:42 UTC

    private static byte[] date = ("\r\nDate:" + dateFormat.format(new Date())).getBytes();

    public static byte[] getDate() {
        return date;
    }

    public static void start() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                while (true) {
                    date = ("\r\nDate:" + dateFormat.format(new Date())).getBytes();
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException e) {

                    }
                }
            }
        }).start();
    }

}
