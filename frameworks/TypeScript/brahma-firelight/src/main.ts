import { createApp, type Response, type Request, type App, type NextFunction, type Handler } from "brahma-firelight";
import cluster from "node:cluster";
import os from "node:os";

const app: App = createApp();

// Server Config Middleware
const serverInfo: Handler = (_req: Request, res: Response, next?: NextFunction) => {
    res.setHeader("Server", "brahma-firelight");
    res.setHeader("Connection", "keep-alive")
    next?.();
};

// JSON compute
app.get("/json", serverInfo, (_req: Request, res: Response) => {
    res.json({ message: "Hello, World!" });
});

// PLAIN-TEXT
app.get("/plaintext", serverInfo, (_req: Request, res: Response) => {
    res.text("Hello, World!");
});

// Port & Host 
const PORT = process.env.PORT || 8080;
const HOST = process.env.HOST || "0.0.0.0";

// Single CORE
//app.listen(HOST, +PORT);

// Multi CORE (cluster with max cpus available)
if (cluster.isPrimary) {
    const cpuCount = os.cpus().length;
    console.log(`Primary ${process.pid} running → forking ${cpuCount} workers...`);

    for (let i = 0; i < cpuCount; i++) {
        cluster.fork();
    }

    cluster.on("exit", (worker) => {
        console.log(`Worker ${worker.process.pid} died → restarting...`);
        cluster.fork();
    });
} else {
    app.listen(HOST, +PORT);
}

// // Enable built in Graceful Shutdown (optional for production use)

// process.on('SIGINT', async () => {
//     console.log('SIGINT → shutting down...');
//     await app.close(2000); // wait up to 2s for requests
//     process.exit(0);
// });

// process.on('SIGTERM', async () => {
//     console.log('SIGTERM → shutting down...');
//     await app.close(2000);
//     process.exit(0);
// });