import os from 'node:os';

const numCPUs = os.cpus().length;
for (let i = 0; i < numCPUs; i++) {
  Bun.spawn(['bun', 'dist/main.bun.js'], {
    stdio: ['inherit', 'inherit', 'inherit'],
    env: { ...process.env },
  });
}
