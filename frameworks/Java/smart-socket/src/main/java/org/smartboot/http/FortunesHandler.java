package org.smartboot.http;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.runtime.RuntimeConstants;
import org.apache.velocity.runtime.resource.loader.ClasspathResourceLoader;
import org.smartboot.http.server.HttpRequest;
import org.smartboot.http.server.HttpResponse;
import org.smartboot.http.server.HttpServerHandle;

import javax.sql.DataSource;
import java.io.IOException;
import java.io.StringWriter;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

/**
 * @author 三刀（zhengjunweimail@163.com）
 * @version V1.0 , 2021/1/24
 */
public class FortunesHandler extends HttpServerHandle {
    private final DataSource dataSource;
    private final Template template;

    public FortunesHandler(DataSource dataSource) {
        this.dataSource = dataSource;
        // 初始化模板引擎
        VelocityEngine ve = new VelocityEngine();
        ve.setProperty(RuntimeConstants.RESOURCE_LOADER, "classpath");
        ve.setProperty("output.encoding", "utf8");
        ve.setProperty("input.encoding", "utf8");
        ve.setProperty("classpath.resource.loader.class", ClasspathResourceLoader.class.getName());
        ve.setProperty("eventhandler.referenceinsertion.class", "org.apache.velocity.app.event.implement.EscapeHtmlReference");
        ve.setProperty("eventhandler.escape.html.match", "/^(?!\\$\\!?unesc_).*/");
        ve.init();
        // 获取模板文件
        template = ve.getTemplate("fortunes.vm", "utf8");
    }

    public static void main(String[] args) {
        // 初始化模板引擎
        VelocityEngine ve = new VelocityEngine();
        ve.setProperty(RuntimeConstants.RESOURCE_LOADER, "classpath");
        ve.setProperty("output.encoding", "MS932");
        ve.setProperty("input.encoding", "MS932");
        ve.setProperty("classpath.resource.loader.class", ClasspathResourceLoader.class.getName());
        ve.setProperty("eventhandler.referenceinsertion.class", "org.apache.velocity.app.event.implement.EscapeHtmlReference");
        ve.setProperty("eventhandler.escape.html.match", "/^(?!\\$\\!?unesc_).*/");
        ve.init();
        // 获取模板文件
        Template template = ve.getTemplate("fortunes.vm", "MS932");
        VelocityContext ctx = new VelocityContext();
        List<Fortune> fortunes = new ArrayList<>();
        fortunes.add(new Fortune(1, "<ab>"));
        ctx.put("fortunes", fortunes);
        ctx.put("text", "フレームワークのベンチマーク");
        StringWriter sw = new StringWriter();
        template.merge(ctx, sw);
        System.out.println(sw);
        System.out.println("フレームワークのベンチマーク");
    }

    @Override
    public void doHandle(HttpRequest request, HttpResponse response) throws IOException {
        List<Fortune> fortunes = new ArrayList<>();
        try (Connection connection = dataSource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement("SELECT * FROM fortune");
             ResultSet resultSet = preparedStatement.executeQuery();) {
            while (resultSet.next()) {
                fortunes.add(new Fortune(resultSet.getInt(1), resultSet.getString(2)));
            }
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            fortunes.sort(Comparator.comparing(fortune -> fortune.message));
        } catch (SQLException throwables) {
            throwables.printStackTrace();
        }
        VelocityContext ctx = new VelocityContext();
        ctx.put("fortunes", fortunes);
        StringWriter sw = new StringWriter();
        template.merge(ctx, sw);
        byte[] bytes = sw.toString().getBytes("utf8");
        response.setContentLength(bytes.length);
        response.setContentType("text/plain; charset=UTF-8");
        response.write(bytes);
    }

    public static final class Fortune {
        public final int id;
        public final String message;

        public Fortune(int id, String message) {
            this.id = id;
            this.message = Objects.requireNonNull(message);
        }

        public int getId() {
            return id;
        }

        public String getMessage() {
            return message;
        }
    }
}
