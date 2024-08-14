const cpus = navigator.hardwareConcurrency;
const buns = new Array(cpus);

for (let i = 0; i < cpus; i++) {
  buns[i] = Bun.spawn(["./server"], {
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