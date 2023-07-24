import { cpus } from 'node:os'

for (let i = 0; i < cpus().length; i++) {
    const proc = Bun.spawn(["bun", "run", "app.ts"], {
        env: process.env,
        onExit(proc, exitCode, signalCode, error) {
            if (error) {
                console.error(error);
            }
        },
    })
    console.log(`[${proc.pid}] Bun is ready`)
}