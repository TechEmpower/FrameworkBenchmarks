/*
 | Mix Asset Management
 |
 | Mix provides a clean, fluent API for defining some Webpack build steps
 | for your application.
 |
 | Docs: https://github.com/JeffreyWay/laravel-mix/tree/master/docs#readme
 */

let mix = require("laravel-mix");

mix
  // JS entry file. Supports Vue, and uses Babel
  //
  // More info and options (like React support) here:
  // https://github.com/JeffreyWay/laravel-mix/blob/master/docs/mixjs.md
  .js("src/js/app.js", "public/js")
  // SASS entry file. Uses autoprefixer automatically.
  .sass("src/css/app.scss", "public/css")
  // Customize postCSS:
  // https://github.com/JeffreyWay/laravel-mix/blob/master/docs/css-preprocessors.md#postcss-plugins
  .options({
    // http://lostgrid.org Simple and powerful CSS grids
    postCss: [require("lost")],
    // If you want to process images, change this to true and add options from
    // https://github.com/tcoopman/image-webpack-loader
    imgLoaderOptions: { enabled: false },
    // Stops Mix from clearing the console when compilation succeeds
    clearConsole: false
  })
  // Set public path so manifest gets output here
  .setPublicPath("public")
  // Add assets to the manifest
  .version(["public/assets"])
  // Silence most output from webpack
  .webpackConfig({ stats: "errors-only" })
  .disableSuccessNotifications();

// Full API
// Docs: https://github.com/JeffreyWay/laravel-mix/tree/master/docs#readme
//
// mix.js(src, output);
// mix.react(src, output); <-- Identical to mix.js(), but registers React Babel compilation.
// mix.preact(src, output); <-- Identical to mix.js(), but registers Preact compilation.
// mix.coffee(src, output); <-- Identical to mix.js(), but registers CoffeeScript compilation.
// mix.ts(src, output); <-- TypeScript support. Requires tsconfig.json to exist in the same folder as webpack.mix.js
// mix.extract(vendorLibs);
// mix.sass(src, output);
// mix.standaloneSass('src', output); <-- Faster, but isolated from Webpack.
// mix.fastSass('src', output); <-- Alias for mix.standaloneSass().
// mix.less(src, output);
// mix.stylus(src, output);
// mix.postCss(src, output, [require('postcss-some-plugin')()]);
// mix.browserSync('my-site.test');
// mix.combine(files, destination);
// mix.babel(files, destination); <-- Identical to mix.combine(), but also includes Babel compilation.
// mix.copy(from, to);
// mix.copyDirectory(fromDir, toDir);
// mix.minify(file);
// mix.sourceMaps(); // Enable sourcemaps
// mix.version(); // Enable versioning.
// mix.disableNotifications();
// mix.disableSuccessNotifications();
// mix.setPublicPath('path/to/public');
// mix.setResourceRoot('prefix/for/resource/locators');
// mix.autoload({}); <-- Will be passed to Webpack's ProvidePlugin.
// mix.webpackConfig({}); <-- Override webpack.config.js, without editing the file directly.
// mix.babelConfig({}); <-- Merge extra Babel configuration (plugins, etc.) with Mix's default.
// mix.then(function () {}) <-- Will be triggered each time Webpack finishes building.
// mix.extend(name, handler) <-- Extend Mix's API with your own components.
// mix.options({
//   extractVueStyles: false, // Extract .vue component styling to file, rather than inline.
//   globalVueStyles: file, // Variables file to be imported in every component.
//   processCssUrls: true, // Process/optimize relative stylesheet url()'s. Set to false, if you don't want them touched.
//   purifyCss: false, // Remove unused CSS selectors.
//   uglify: {}, // Uglify-specific options. https://webpack.github.io/docs/list-of-plugins.html#uglifyjsplugin
//   postCss: [] // Post-CSS options: https://github.com/postcss/postcss/blob/master/docs/plugins.md
// });
