const cluster = require("node:cluster");
const os = require("node:os");
const process = require("node:process");
const { SimpleRouter, createServer } = require("velocy");

if (cluster.isPrimary) {
    console.log(`Primary ${process.pid} is running`);

    const numCPUs = os.cpus().length;
    for (let i = 0; i < numCPUs; i++) {
        cluster.fork();
    }

    cluster.on("exit", (worker) => {
        console.log(`worker ${worker.process.pid} died`);
        process.exit(1);
    });
} else {
    const router = new SimpleRouter();

    router.get("/plaintext", (req, res) => {
        let p = "Hello, World!";
        res.writeHead(200, {
            "content-type": "text/plain",
            "content-length": p.length,
            Server: "Velocy",
        });
        res.end(p);
    });

    router.get("/json", (req, res) => {
        let p = JSON.stringify({ message: "Hello, World!" });

        res.writeHead(200, {
            "content-type": "application/json",
            "content-length": p.length,
            Server: "Velocy",
        });

        res.end(p);
    });

    createServer(router).listen(8080);

    console.log(`Worker ${process.pid} started`);
}
