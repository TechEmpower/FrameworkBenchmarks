import cluster from "node:cluster";
import { availableParallelism } from "node:os";

if (cluster.isPrimary) {
  const cpus = availableParallelism();
  console.log(`x-gnosis: spawning ${cpus} workers`);

  for (let i = 0; i < cpus; i++) {
    cluster.fork();
  }

  cluster.on("exit", (worker, code) => {
    console.error(`worker ${worker.process.pid} exited (${code}), restarting`);
    cluster.fork();
  });
} else {
  await import("./index.js");
}
