import { cpus } from 'node:os';

for (let i = 0; i < cpus().length; i++) {
  Bun.spawn(['bun', 'run', 'src/app.ts'], {
    env: process.env,
    stdout: 'inherit',
    onExit(proc, exitCode, signalCode, error) {
      if (error) {
        console.error(error);
      }
    },
  });
}
