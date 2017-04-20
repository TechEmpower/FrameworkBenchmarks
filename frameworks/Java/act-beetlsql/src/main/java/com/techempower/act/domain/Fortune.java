package com.techempower.act.domain;


import act.util.SimpleBean;
import org.beetl.sql.core.mapper.BaseMapper;
import org.jetbrains.annotations.NotNull;

import javax.persistence.Entity;
import javax.persistence.Id;

@Entity
public class Fortune implements SimpleBean, Comparable<Fortune> {

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

    public void setId(Integer id) {
        this.id = id;
    }

    public String getMessage() {
        return this.message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public int compareTo(@NotNull Fortune o) {
        return getMessage().compareTo(o.getMessage());
    }

    public interface Dao extends BaseMapper<Fortune> {}
}
