const { STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO } = just.sys
const path = just.sys.cwd()
const [...args] = just.args.slice(2)
const cpus = parseInt(just.env().CPUS || just.sys.cpus, 10)
for (let i = 0; i < cpus; i++) just.sys.spawn('./just', path, args, STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO)
while (1) just.sys.sleep(1)
