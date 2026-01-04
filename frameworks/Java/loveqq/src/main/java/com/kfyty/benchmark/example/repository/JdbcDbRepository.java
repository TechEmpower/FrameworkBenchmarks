package com.kfyty.benchmark.example.repository;

import com.kfyty.benchmark.example.model.Fortune;
import com.kfyty.benchmark.example.model.World;
import com.kfyty.loveqq.framework.core.autoconfig.annotation.Repository;
import com.kfyty.loveqq.framework.core.exception.ResolvableException;
import com.kfyty.loveqq.framework.core.generic.SimpleGeneric;
import com.kfyty.loveqq.framework.core.reflect.DefaultParameterizedType;
import com.kfyty.loveqq.framework.core.utils.JdbcUtil;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;

@Repository
public class JdbcDbRepository implements DbRepository {
    private final DataSource dataSource;

    public JdbcDbRepository(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public World getWorld(int id) {
        try {
            return JdbcUtil.query(dataSource, World.class, "SELECT id, randomnumber FROM world WHERE id = ?", id);
        } catch (SQLException e) {
            return null;
        }
    }

    @Override
    public void updateWorlds(List<World> worlds) {
        try {
            for (World world : worlds) {
                JdbcUtil.execute(dataSource, "UPDATE world SET randomnumber = ? WHERE id = ?", world.randomNumber, world.id);
            }
        } catch (SQLException e) {
            throw new ResolvableException(e);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public List<Fortune> fortunes() {
        try {
            DefaultParameterizedType parameterizedType = new DefaultParameterizedType(List.class, new Class<?>[]{Fortune.class});
            Object queried = JdbcUtil.query(dataSource, SimpleGeneric.from(parameterizedType), "SELECT id, message FROM fortune");
            return (List<Fortune>) queried;
        } catch (SQLException e) {
            return new LinkedList<>();
        }
    }
}
