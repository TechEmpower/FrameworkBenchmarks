/// Your very own web application!
import 'dart:async';
import 'package:angel3_framework/angel3_framework.dart';
import 'package:file/local.dart';
import 'src/config/config.dart' as configuration;
import 'src/routes/routes.dart' as routes;
import 'src/services/services.dart' as services;

/// Configures the server instance.
Future configureServer(Angel app) async {
  // Grab a handle to the file system, so that we can do things like
  // serve static files.
  var fs = const LocalFileSystem();

  // Set up our application, using the plug-ins defined with this project.
  await app.configure(configuration.configureServer(fs));
  await app.configure(services.configureServer);
  await app.configure(routes.configureServer(fs));
}
