const numCPUs = navigator.hardwareConcurrency;
for (let i = 0; i < numCPUs; i++) {
  Bun.spawn(["./server"], {
    stdio: ["inherit", "inherit", "inherit"],
    env: { ...process.env },
  });
}
