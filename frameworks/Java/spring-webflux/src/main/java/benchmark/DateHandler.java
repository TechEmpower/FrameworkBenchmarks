package benchmark;

import org.springframework.scheduling.annotation.Scheduled;

import java.util.Date;

public class DateHandler {

    private Date date = new Date();

    @Scheduled(fixedRate = 1000)
    public void update() {
        this.date = new Date();
    }

    public Date getDate() {
        return date;
    }
}
