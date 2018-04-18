/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.util;

import java.lang.reflect.*;
import java.net.*;
import java.nio.file.Paths;
import java.util.HashSet;

/**
 *
 * @author zhangjx
 */
public class RedkaleClassLoader extends URLClassLoader {

    public RedkaleClassLoader(ClassLoader parent) {
        super(new URL[0], parent);
    }

    public Class<?> loadClass(String name, byte[] b) {
        return defineClass(name, b, 0, b.length);
    }

    @Override
    public void addURL(URL url) {
        super.addURL(url);
    }

    @Override
    public URL[] getURLs() {
        return super.getURLs();
    }

    public URL[] getAllURLs() {
        ClassLoader loader = this;
        HashSet<URL> set = new HashSet<>();
        String appPath = System.getProperty("java.class.path");
        if (appPath != null && !appPath.isEmpty()) {
            for (String path : appPath.replace(":/", "&&").replace(":\\", "##").replace(':', ';').split(";")) {
                try {
                    set.add(Paths.get(path.replace("&&", ":/").replace("##", ":\\")).toRealPath().toFile().toURI().toURL());
                } catch (Exception e) {
                }
            }
        }
        do {
            String loaderName = loader.getClass().getName();
            if (loaderName.startsWith("sun.") && loaderName.contains("ExtClassLoader")) continue;
            if (loader instanceof URLClassLoader) {
                for (URL url : ((URLClassLoader) loader).getURLs()) {
                    set.add(url);
                }
            } else { //可能JDK9及以上
                loader.getResource("org.redkale"); //必须要运行一次，确保URLClassPath的值被填充完毕
                Class loaderClazz = loader.getClass();
                Object ucp = null;
                do { //读取 java.base/jdk.internal.loader.BuiltinClassLoader的URLClassPath ucp值
                    try {
                        //需要在命令行里加入：  --add-opens java.base/jdk.internal.loader=ALL-UNNAMED
                        Field field = loaderClazz.getDeclaredField("ucp");
                        field.setAccessible(true);
                        ucp = field.get(loader);
                        break;
                    } catch (Throwable e) {
                    }
                } while ((loaderClazz = loaderClazz.getSuperclass()) != Object.class);
                if (ucp != null) { //URLClassPath
                    URL[] urls = null;
                    try {  //读取 java.base/jdk.internal.loader.URLClassPath的urls值
                        Method method = ucp.getClass().getMethod("getURLs");
                        urls = (URL[]) method.invoke(ucp);
                    } catch (Exception e) {
                    }
                    if (urls != null) {
                        for (URL url : urls) {
                            set.add(url);
                        }
                    }
                }
            }
        } while ((loader = loader.getParent()) != null);
        return set.toArray(new URL[set.size()]);
    }
}
