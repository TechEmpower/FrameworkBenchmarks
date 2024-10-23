import cluster from "node:cluster";
import os from "node:os";
import process from "node:process";

if (cluster.isPrimary) {
  console.log(`Primary ${process.pid} is running`);

  const numCPUs = os.availableParallelism();
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on("exit", (worker) => {
    console.log(`worker ${worker.process.pid} died`);
    process.exit(1);
  });
} else {
  await import("./server");
  console.log(`Worker ${process.pid} started`);
}
