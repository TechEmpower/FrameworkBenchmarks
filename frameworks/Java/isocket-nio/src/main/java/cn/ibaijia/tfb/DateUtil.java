package cn.ibaijia.tfb;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

public class DateUtil {

    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss z", Locale.ENGLISH);//Fri, 09 Jul 2021 09:10:42 UTC

    private static String date = dateFormat.format(new Date());

    public static String getDate() {
        return date;
    }

    public static void start() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                while (true) {
                    date = dateFormat.format(new Date());
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException e) {

                    }
                }
            }
        }).start();
    }

}
