package com.test.hserver.controller;

import cn.hserver.core.ioc.annotation.Autowired;
import cn.hserver.plugin.web.annotation.Controller;
import cn.hserver.plugin.web.annotation.GET;
import cn.hserver.plugin.web.interfaces.HttpResponse;
import com.test.hserver.bean.Fortune;
import com.test.hserver.bean.Message;
import com.test.hserver.bean.World;
import com.test.hserver.util.DateUtil;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;

import static com.test.hserver.util.Util.getQueries;
import static com.test.hserver.util.Util.randomWorld;

/**
 * @author hxm
 */
@Controller
public class TestController {
    private static final String HELLO = "Hello, World!";
    private static final String SELECT_WORLD = "select * from world where id=?";

    @Autowired
    private DataSource dataSource;

    @GET("/json")
    public Message json(HttpResponse response) {
        response.setHeader("Date", DateUtil.getTime());
        return new Message();
    }

    @GET("/plaintext")
    public String plaintext(HttpResponse response) {
        response.setHeader("Date", DateUtil.getTime());
        return HELLO;
    }

    @GET("/db")
    public void db(HttpResponse response) throws SQLException {
        World result;
        try (Connection conn = dataSource.getConnection()) {
            try (final PreparedStatement statement = conn.prepareStatement(SELECT_WORLD)) {
                statement.setInt(1, randomWorld());
                try (ResultSet rs = statement.executeQuery()) {
                    rs.next();
                    result = new World(rs.getInt("id"), rs.getInt("randomNumber"));
                }
            }
        }
        response.setHeader("Date", DateUtil.getTime());
        response.sendJson(result);
    }

    @GET("/queries")
    public void queries(String queries,HttpResponse response) throws Exception {
        World[] result = new World[getQueries(queries)];
        try (Connection conn = dataSource.getConnection()) {
            for (int i = 0; i < result.length; i++) {
                try (final PreparedStatement statement = conn.prepareStatement(SELECT_WORLD)) {
                    statement.setInt(1, randomWorld());
                    try (ResultSet rs = statement.executeQuery()) {
                        rs.next();
                        result[i] = new World(rs.getInt("id"), rs.getInt("randomNumber"));
                    }
                }
            }
        }
        response.setHeader("Date", DateUtil.getTime());
        response.sendJson(result);
    }


    @GET("/updates")
    public void updates(String queries,HttpResponse response) throws Exception {
        World[] result = new World[getQueries(queries)];
        StringJoiner updateSql = new StringJoiner(
                ", ",
                "UPDATE world SET randomNumber = temp.randomNumber FROM (VALUES ",
                " ORDER BY 1) AS temp(id, randomNumber) WHERE temp.id = world.id");

        try (Connection connection = dataSource.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(SELECT_WORLD)) {
                for (int i = 0; i < result.length; i++) {
                    statement.setInt(1, randomWorld());
                    try (ResultSet rs = statement.executeQuery()) {
                        rs.next();
                        result[i] = new World(rs.getInt("id"), randomWorld());
                    }
                    // prepare update query
                    updateSql.add("(?, ?)");
                }
            }

            try (PreparedStatement statement = connection.prepareStatement(updateSql.toString())) {
                int i = 0;
                for (World world : result) {
                    statement.setInt(++i, world.getRandomNumber());
                    statement.setInt(++i, world.getRandomNumber());
                }
                statement.executeUpdate();
            }
        }
        response.setHeader("Date", DateUtil.getTime());
        response.sendJson(result);
    }

    @GET("/fortunes")
    public void fortunes(HttpResponse response) throws Exception {
        List<Fortune> fortunes = new ArrayList<>();
        try (Connection connection = dataSource.getConnection()) {
            try (PreparedStatement stt = connection.prepareStatement("select * from fortune")) {
                try (ResultSet rs = stt.executeQuery()) {
                    while (rs.next()) {
                        fortunes.add(new Fortune(rs.getInt("id"), rs.getString("message")));
                    }
                }
            }
        }
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        response.setHeader("Date", DateUtil.getTime());
        Map<String,Object> data=new HashMap<>();
        data.put("data",fortunes);
        response.sendTemplate("fortunes.ftl",data);
    }
}
