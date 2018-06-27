package micronaut;

import io.micronaut.scheduling.annotation.Scheduled;

import javax.annotation.PostConstruct;
import javax.inject.Singleton;

@Singleton
public class DateHandler {
    private CharSequence date;

    public CharSequence getDate() {
        return date;
    }

    @Scheduled(fixedRate = "1s")
    public void refreshDate() {
        this.date = java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.format(java.time.ZonedDateTime.now());
    }

    @PostConstruct
    void init() {
        refreshDate();
    }
}
