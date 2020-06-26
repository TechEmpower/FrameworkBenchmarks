import * as cluster from "cluster";
import * as os from "os";
import Server from "./server";

if (cluster.isMaster) {
  const cpuCount: os.CpuInfo[] = os.cpus();

  for (const cpu of cpuCount) {
    cluster.fork();
  }

  cluster.on("exit", () => {
    process.exit(1);
  });
} else {
  new Server().start();
}
