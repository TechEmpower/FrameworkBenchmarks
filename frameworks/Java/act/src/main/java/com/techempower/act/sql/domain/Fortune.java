package com.techempower.act.sql.domain;

import act.data.annotation.Data;
import act.util.SimpleBean;
import com.techempower.act.domain.IFortune;

import javax.persistence.Id;
import javax.persistence.MappedSuperclass;

@Data
@MappedSuperclass
public class Fortune implements IFortune, SimpleBean {

    @Id
    private Integer id;

    private String message;

    public Fortune(Integer id, String message) {
        this.id = id;
        this.message = message;
    }

    public Integer getId() {
        return id;
    }

    public String getMessage() {
        return this.message;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}
