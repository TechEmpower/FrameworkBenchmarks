package com.litongjava.tio.http.server.utils;

import java.util.HashMap;
import java.util.Map;

public class EnviormentUtils {
  private static String[] args;
  private static Map<String, String> cmdArgsMap = new HashMap<>();

  public static String[] getArgs() {
    return args;
  }

  public Map<String, String> getCmdArgsMap() {
    return cmdArgsMap;
  }

  public static Map<String, String> buildCmdArgsMap(String[] args) {
    EnviormentUtils.args = args;
    Map<String, String> result = new HashMap<>();
    for (String arg : args) {
      if (arg.startsWith("--")) {
        String[] parts = arg.substring(2).split("=", 2);
        if (parts.length == 2) {
          result.put(parts[0], parts[1]);
        }
      }
    }
    cmdArgsMap = result;
    return result;
  }

  public static String getStr(String key) {
    // comamdn line
    String value = cmdArgsMap.get(key);
    // enviorment
    if (value == null) {
      value = System.getProperty(key);
    }

    // config file
    if (value == null) {
//      if (PropUtils.isLoad()) {
//        value = PropUtils.get(key);
//      }

    }

    return value;
  }

  public static String get(String key) {
    return getStr(key);
  }

  public static Integer getInt(String key) {
    String value = getStr(key);
    if (value != null) {
      return Integer.valueOf(value);
    } else {
      return null;
    }

  }

  public static int getInt(String key, int defaultValue) {
    String value = get(key);
    if (value != null) {
      return Integer.parseInt(value);
    } else {
      return defaultValue;
    }
  }

  public static String get(String key, String defaultValue) {
    String value = get(key);
    if (value != null) {
      return value;
    } else {
      return defaultValue;
    }
  }

  public static boolean getBoolean(String key) {
    return Boolean.parseBoolean(get(key));
  }

  public static boolean getBoolean(String key, boolean defaultValue) {
    String value = get(key);
    if (value != null) {
      return Boolean.parseBoolean(value);
    } else {
      return defaultValue;
    }
  }

//  public static void load(String profile) {
//    if (PropUtils.isLoad()) {
//      PropUtils.use("app-" + profile + ".properties");
//    } else {
//      PropUtils.use(profile);

//    }

//  }
}
