package cn.taketoday.benchmark;

import infra.app.Application;
import infra.app.InfraApplication;

@InfraApplication
public class BenchmarkApplication {

  public static void main(String[] args) {
    Application.run(BenchmarkApplication.class, args);
  }

}
