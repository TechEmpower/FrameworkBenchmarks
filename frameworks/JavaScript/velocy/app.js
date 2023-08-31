const cluster = require("node:cluster");
const os = require("node:os");
const process = require("node:process");
const { Router, createServer } = require("velocy");

const CONTENT_TYPE_STR = "Content-Type";
const CONTENT_LENGTH_STR = "Content-Length";
const RESPONSE_STR = "Hello, world!";
const RESPONSE_LENGTH = Buffer.byteLength(RESPONSE_STR);
const FRAMEWORK_NAME = "Velocy";


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
    const router = new Router();
    
    router.get("/plaintext", (req, res) => {
        res.writeHead(200, {
            [CONTENT_TYPE_STR]: "text/plain",
            [CONTENT_LENGTH_STR]: RESPONSE_LENGTH,
            Server: FRAMEWORK_NAME,
        });
        res.end("Hello, World!");
    });

    createServer(router).listen(8080);

    console.log(`Worker ${process.pid} started`);
}
