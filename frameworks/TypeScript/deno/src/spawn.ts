import os from "node:os";
import process from "node:process";

const numCPUs = os.cpus().length;
for (let i = 0; i < numCPUs; i++) {
  new Deno.Command(Deno.execPath(), {
    args: ["run", "-A", "--unstable-net", "main.ts"],
    stdin: "inherit",
    stdout: "inherit",
    stderr: "inherit",
    env: { ...process.env },
  }).spawn();
}
