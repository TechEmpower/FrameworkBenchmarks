/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.io.*;
import java.lang.reflect.Constructor;
import java.net.URL;
import java.util.*;
import javax.xml.stream.*;

/**
 *
 * @author zhangjx
 */
public final class DataSources {

    public static final String DATASOURCE_CONFPATH = "DATASOURCE_CONFPATH";

    public static final String JDBC_DATASOURCE_CLASS = "javax.persistence.datasource";

    public static final String JDBC_CACHE_MODE = "javax.persistence.cachemode";

    public static final String JDBC_CONNECTIONSMAX = "javax.persistence.connections.limit";

    public static final String JDBC_CONTAIN_SQLTEMPLATE = "javax.persistence.contain.sqltemplate";

    public static final String JDBC_NOTCONTAIN_SQLTEMPLATE = "javax.persistence.notcontain.sqltemplate";

    public static final String JDBC_TABLENOTEXIST_SQLSTATES = "javax.persistence.tablenotexist.sqlstates";

    public static final String JDBC_TABLECOPY_SQLTEMPLATE = "javax.persistence.tablecopy.sqltemplate";

    public static final String JDBC_CONNECTTIMEOUT_SECONDS = "javax.persistence.connecttimeout";

    public static final String JDBC_READTIMEOUT_SECONDS = "javax.persistence.readtimeout";

    public static final String JDBC_WRITETIMEOUT_SECONDS = "javax.persistence.writetimeout";

    public static final String JDBC_URL = "javax.persistence.jdbc.url";

    public static final String JDBC_USER = "javax.persistence.jdbc.user";

    public static final String JDBC_PWD = "javax.persistence.jdbc.password";

    public static final String JDBC_DRIVER = "javax.persistence.jdbc.driver";

    public static final String JDBC_SOURCE = "javax.persistence.jdbc.source";

    private DataSources() {
    }

    public static DataSource createDataSource(final String unitName, Properties prop) throws IOException {
        return new DataJdbcSource(unitName, prop, prop);
    }

    public static DataSource createDataSource(final String unitName, Properties readprop, Properties writeprop) throws IOException {
        return new DataJdbcSource(unitName, readprop, writeprop);
    }

    public static DataSource createDataSource(final String unitName) throws IOException {
        return createDataSource(unitName, System.getProperty(DATASOURCE_CONFPATH) == null
            ? DataJdbcSource.class.getResource("/META-INF/persistence.xml")
            : new File(System.getProperty(DATASOURCE_CONFPATH)).toURI().toURL());
    }

    public static DataSource createDataSource(final String unitName, URL url) throws IOException {
        if (url == null) url = DataSources.class.getResource("/persistence.xml");
        InputStream in = url.openStream();
        if (in == null) return null;
        Map<String, Properties> map = loadPersistenceXml(in);
        Properties readprop = null;
        Properties writeprop = null;
        if (unitName != null) {
            readprop = map.get(unitName);
            writeprop = readprop;
            if (readprop == null) {
                readprop = map.get(unitName + ".read");
                writeprop = map.get(unitName + ".write");
            }
        }
        if ((unitName == null || unitName.isEmpty()) || readprop == null) {
            String key = null;
            for (Map.Entry<String, Properties> en : map.entrySet()) {
                key = en.getKey();
                readprop = en.getValue();
                writeprop = readprop;
                break;
            }
            if (key != null && (key.endsWith(".read") || key.endsWith(".write"))) {
                if (key.endsWith(".read")) {
                    writeprop = map.get(key.substring(0, key.lastIndexOf('.')) + ".write");
                } else {
                    readprop = map.get(key.substring(0, key.lastIndexOf('.')) + ".read");
                }
            }
        }
        if (readprop == null) throw new IOException("Cannot find (resource.name = '" + unitName + "') DataSource");
        if (writeprop == null) writeprop = readprop;
        String impl = readprop.getProperty(JDBC_DATASOURCE_CLASS, DataJdbcSource.class.getName());
        if (DataJdbcSource.class.getName().equals(impl)) return new DataJdbcSource(unitName, readprop, writeprop);
        try {
            Class ds = Thread.currentThread().getContextClassLoader().loadClass(impl);
            for (Constructor d : ds.getConstructors()) {
                Class<?>[] paramtypes = d.getParameterTypes();
                if (paramtypes.length == 1 && paramtypes[0] == Properties.class) {
                    return (DataSource) d.newInstance(readprop);
                } else if (paramtypes.length == 2 && paramtypes[0] == String.class && paramtypes[1] == Properties.class) {
                    return (DataSource) d.newInstance(unitName, readprop);
                } else if (paramtypes.length == 3 && paramtypes[0] == String.class && paramtypes[1] == Properties.class && paramtypes[2] == Properties.class) {
                    return (DataSource) d.newInstance(unitName, readprop, writeprop);
                }
            }
            throw new IOException("DataSource impl class (" + impl + ") have no Constructor by (Properties prop) or  (String name, Properties prop) or  (String name, Properties readprop, Propertieswriteprop)");
        } catch (IOException ex) {
            throw ex;
        } catch (Exception e) {
            throw new IOException(e);
        }
    }

    public static Map<String, Properties> loadPersistenceXml(final InputStream in0) {
        final Map<String, Properties> map = new LinkedHashMap();
        Properties result = new Properties();
        boolean flag = false;
        try (final InputStream in = in0) {
            XMLStreamReader reader = XMLInputFactory.newFactory().createXMLStreamReader(in);
            while (reader.hasNext()) {
                int event = reader.next();
                if (event == XMLStreamConstants.START_ELEMENT) {
                    if ("persistence-unit".equalsIgnoreCase(reader.getLocalName())) {
                        if (!result.isEmpty()) result = new Properties();
                        map.put(reader.getAttributeValue(null, "name"), result);
                        flag = true;
                    } else if (flag && "property".equalsIgnoreCase(reader.getLocalName())) {
                        String name = reader.getAttributeValue(null, "name");
                        String value = reader.getAttributeValue(null, "value");
                        if (name == null) continue;
                        result.put(name, value);
                    } else if (flag && "shared-cache-mode".equalsIgnoreCase(reader.getLocalName())) { //兼容shared-cache-mode属性
                        result.put(JDBC_CACHE_MODE, reader.getElementText());
                    }
                }
            }
            in.close();
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
        return map;
    }
}
