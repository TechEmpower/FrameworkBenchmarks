/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.boot.watch;

import java.io.IOException;
import java.util.List;
import javax.annotation.Resource;
import org.redkale.boot.*;
import org.redkale.net.http.*;
import org.redkale.service.RetResult;
import org.redkale.util.Comment;

/**
 *
 * @author zhangjx
 */
@RestService(name = "filter", catalog = "watch", repair = false)
public class FilterWatchService extends AbstractWatchService {

    @Comment("Filter类名不存在")
    public static final int RET_FILTER_TYPE_NOT_EXISTS = 1601_0002;

    @Comment("Filter类名不合法")
    public static final int RET_FILTER_TYPE_ILLEGAL = 1601_0003;

    @Comment("Filter类名已存在")
    public static final int RET_FILTER_EXISTS = 1601_0004;

    @Comment("Filter的JAR包不存在")
    public static final int RET_FILTER_JAR_ILLEGAL = 1601_0005;

    @Resource
    private Application application;

    @RestMapping(name = "addfilter", auth = false, comment = "动态增加Filter")
    public RetResult addFilter(@RestUploadFile(maxLength = 10 * 1024 * 1024, fileNameReg = "\\.jar$") byte[] jar,
        @RestParam(name = "server", comment = "Server节点名") final String serverName,
        @RestParam(name = "type", comment = "Filter类名") final String filterType) throws IOException {
        if (filterType == null) return new RetResult(RET_FILTER_TYPE_NOT_EXISTS, "Not found Filter Type (" + filterType + ")");
        if (jar == null) return new RetResult(RET_FILTER_JAR_ILLEGAL, "Not found jar file");
        List<NodeServer> nodes = application.getNodeServers();
        for (NodeServer node : nodes) {
            if (node.getServer().containsFilter(filterType)) return new RetResult(RET_FILTER_EXISTS, "Filter(" + filterType + ") exists");
        }
        return RetResult.success();
    }
}
