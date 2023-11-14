import os from 'node:os';

// @ts-ignore
const numCPUs = os.availableParallelism();
for (let i = 0; i < numCPUs; i++) {
  Bun.spawn(['bun', 'build/index.js'], {
    stdio: ['inherit', 'inherit', 'inherit'],
    env: { ...process.env },
  });
}
