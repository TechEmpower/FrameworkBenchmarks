/// Configuration for this Angel instance.
import 'package:angel3_configuration/angel3_configuration.dart';
import 'package:angel3_framework/angel3_framework.dart';
import 'package:angel3_jael/angel3_jael.dart';
import 'package:file/file.dart';
import 'plugins/plugins.dart' as plugins;

/// This is a perfect place to include configuration and load plug-ins.
AngelConfigurer configureServer(FileSystem fileSystem) {
  return (Angel app) async {
    // Load configuration from the `config/` directory.
    //
    // See: https://github.com/angel-dart/configuration
    await app.configure(configuration(fileSystem));

    // Configure our application to render Jael templates from the `views/` directory.
    //
    // See: https://github.com/angel-dart/jael
    await app.configure(jael(fileSystem.directory('views'), minified: true));

    // Apply another plug-ins, i.e. ones that *you* have written.
    //
    // Typically, the plugins in `lib/src/config/plugins/plugins.dart` are plug-ins
    // that add functionality specific to your application.
    //
    // If you write a plug-in that you plan to use again, or are
    // using one created by the community, include it in
    // `lib/src/config/config.dart`.
    await plugins.configureServer(app);
  };
}
