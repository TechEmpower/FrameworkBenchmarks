import cluster, { isPrimary, setupPrimary, fork } from 'node:cluster'
import { cpus } from 'node:os'

if (isPrimary) {
  setupPrimary({
    exec: 'app.js',
  })
  cluster.on('exit', (worker) => {
    console.log(`worker ${worker.process.pid} died`)
    process.exit(1)
  })
  for (let i = 0; i < cpus().length; i++) {
    fork()
  }
}