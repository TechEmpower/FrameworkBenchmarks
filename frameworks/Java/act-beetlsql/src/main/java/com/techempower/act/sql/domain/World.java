package com.techempower.act.sql.domain;


import act.data.annotation.Data;
import act.util.SimpleBean;
import com.techempower.act.domain.IWorld;
import org.beetl.sql.core.annotatoin.AutoID;
import org.beetl.sql.core.annotatoin.Sql;
import org.beetl.sql.core.mapper.BaseMapper;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;

@Data
@MappedSuperclass
public class World implements IWorld, SimpleBean {

    @Id
    @GeneratedValue
    private Integer id;

    @Column(name = "randomNumber")
    private Integer randomNumber;

    public World(Integer id, Integer randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }

    @AutoID
    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getRandomNumber() {
        return randomNumber;
    }

    public void setRandomNumber(Integer randomNumber) {
        this.randomNumber = randomNumber;
    }

    public interface WorldDao<WORLD_MODEL> extends BaseMapper<WORLD_MODEL> {
        @Sql("update world set randomNumber = ? where id = ?")
        void update(int randomNumber, int id);
    }

}
