const { Application } = require("stick");

const app = exports.app = Application();
app.configure("mount");
app.mount("/", require("./app/views"));

if (require.main == module) {
    require("ringo/httpserver").main(module.id);
}
