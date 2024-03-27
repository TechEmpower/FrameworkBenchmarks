package cn.taketoday.benchmark;

import cn.taketoday.framework.Application;
import cn.taketoday.framework.InfraApplication;

@InfraApplication
public class BenchmarkApplication {

  public static void main(String[] args) {
    Application.run(BenchmarkApplication.class, args);
  }

}
