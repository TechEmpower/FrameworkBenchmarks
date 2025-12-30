package com.test.hserver.controller;

import cn.hserver.core.context.IocApplicationContext;

import cn.hserver.mvc.context.WebContext;
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
public class TestController {
    private static final String HELLO = "Hello, World!";
    private static final String SELECT_WORLD = "select * from world where id=?";

    private static DataSource dataSource;

    public static DataSource getDataSource() {
           if (dataSource == null) {
               dataSource= IocApplicationContext.getBean(DataSource.class);
           }
            return dataSource;
    }


    public static void json(WebContext webContext) {
        webContext.response.addHeader("Date", DateUtil.getTime());
        webContext.response.sendJson(new Message());
    }

    public static void plaintext(WebContext webContext) {
        webContext.response.addHeader("Date", DateUtil.getTime());
        webContext.response.sendText(HELLO);

    }

    public static void db(WebContext webContext) throws SQLException {
        World result;
        try (Connection conn = getDataSource().getConnection()) {
            try (final PreparedStatement statement = conn.prepareStatement(SELECT_WORLD)) {
                statement.setInt(1, randomWorld());
                try (ResultSet rs = statement.executeQuery()) {
                    rs.next();
                    result = new World(rs.getInt("id"), rs.getInt("randomNumber"));
                }
            }
        }
        webContext.response.addHeader("Date", DateUtil.getTime());
        webContext.response.sendJson(result);
    }

    public static void queries(WebContext webContext) throws Exception {
        World[] result = new World[getQueries(webContext.request.query("queries"))];
        try (Connection conn = getDataSource().getConnection()) {
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
        webContext.response.addHeader("Date", DateUtil.getTime());
        webContext.response.sendJson(result);
    }


    public static void updates(WebContext webContext) throws Exception {
        World[] result = new World[getQueries(webContext.request.query("queries"))];
        StringJoiner updateSql = new StringJoiner(
                ", ",
                "UPDATE world SET randomNumber = temp.randomNumber FROM (VALUES ",
                " ORDER BY 1) AS temp(id, randomNumber) WHERE temp.id = world.id");

        try (Connection connection = getDataSource().getConnection()) {
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
        webContext.response.addHeader("Date", DateUtil.getTime());
        webContext.response.sendJson(result);
    }

    public static void fortunes(WebContext webContext) throws Exception {
        List<Fortune> fortunes = new ArrayList<>();
        try (Connection connection = getDataSource().getConnection()) {
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
        webContext.response.addHeader("Date", DateUtil.getTime());
        Map<String,Object> data=new HashMap<>();
        data.put("data",fortunes);
        webContext.response.sendTemplate("fortunes.ftl",data);
    }
}
