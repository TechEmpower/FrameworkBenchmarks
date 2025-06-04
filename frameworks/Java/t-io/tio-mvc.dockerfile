# 第一阶段：使用 Maven 构建并打包
FROM maven:3.9.7-amazoncorretto-21 AS build
WORKDIR /t-io

# 先复制 pom.xml，再复制源代码和脚本目录
COPY pom.xml .
COPY src ./src
COPY script ./script

# 执行 package，会触发 maven-assembly-plugin 生成 target/tio-http-server-benchmark/ 目录，
# 该目录下应包含一个可运行的 JAR（tio-http-server-benchmark.jar）和一个 config 子目录
RUN mvn clean package -q

# 第二阶段：运行时镜像
FROM openjdk:21-jdk-slim
# 切换到与第一阶段输出目录对应的工作目录
WORKDIR /t-io/target/tio-http-server-benchmark
COPY --from=maven /t-io/target/tio-http-server-benchmark/ ./

# 将第一阶段 build 中生成的整个目录复制过来，
# 包含：
#   - tio-http-server-benchmark.jar
#   - config/ （静态配置等）
COPY --from=build /t-io/target/tio-http-server-benchmark/ ./

EXPOSE 8080

# 使用 -cp 指定 JAR 与 config 目录，主类为 TioBenchmarkStarter
CMD ["java", "-server", "-Xms1G", "-Xmx4G", "-cp", "tio-http-server-benchmark.jar:config", "org.tio.http.server.benchmark.TioBenchmarkStarter"]