package com.litongjava.tio.http.server;

import org.junit.Test;

import com.litongjava.tio.utils.environment.EnvUtils;

public class MainAppTest {

  @Test
  public void test() {
    boolean boolean1 = EnvUtils.getBoolean("native", false);
    System.out.println(boolean1);
  }

}
