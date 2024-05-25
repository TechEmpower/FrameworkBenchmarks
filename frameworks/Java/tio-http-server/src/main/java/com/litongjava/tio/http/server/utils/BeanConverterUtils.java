package com.litongjava.tio.http.server.utils;

import java.lang.reflect.Field;
import java.util.Map;

public class BeanConverterUtils {

  /**
   * Map to to bean 
   */
  public static <T> T toBean(Map<String, Object> map, Class<T> beanClass)
      throws IllegalAccessException, InstantiationException {

    T bean = beanClass.newInstance(); // 创建 Bean 的实例

    for (Field field : beanClass.getDeclaredFields()) {
      field.setAccessible(true); // 确保私有字段也可以访问

      if (map.containsKey(field.getName())) {
        Object value = map.get(field.getName());

        // 如果字段类型与值类型兼容，则设置字段的值
        if (value != null && field.getType().isAssignableFrom(value.getClass())) {
          field.set(bean, value);
        }
      }
    }

    return bean;
  }
}
