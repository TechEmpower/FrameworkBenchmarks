import { createApp, type Response, type Request, type App, type NextFunction, type Handler } from "brahma-firelight";

const app: App = createApp();

// Server Config Middleware
const serverInfo: Handler = (_req: Request, res: Response, next?: NextFunction) => {
    res.setHeader("Server", "brahma-firelight");
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

app.listen("0.0.0.0", 8080);

// Enable built in Graceful Shutdown (optional for production use)

process.on('SIGINT', async () => {
    console.log('SIGINT → shutting down...');
    await app.close(2000); // wait up to 2s for requests
    process.exit(0);
});

process.on('SIGTERM', async () => {
    console.log('SIGTERM → shutting down...');
    await app.close(2000);
    process.exit(0);
});