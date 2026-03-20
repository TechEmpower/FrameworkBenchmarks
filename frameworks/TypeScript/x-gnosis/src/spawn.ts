import { existsSync } from "node:fs";

const cpus = navigator.hardwareConcurrency;
const buns = new Array(cpus);

// Use compiled binary if available (default variant), else run index.ts directly (postgresql variant)
const cmd = existsSync("./server") ? ["./server"] : ["bun", "index.ts"];

for (let i = 0; i < cpus; i++) {
  buns[i] = Bun.spawn(cmd, {
    stdio: ["inherit", "inherit", "inherit"],
    env: { ...process.env },
  });
}

function kill() {
  for (const bun of buns) {
    bun.kill();
  }
}

process.on("SIGINT", kill);
process.on("exit", kill);
