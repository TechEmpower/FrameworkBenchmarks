import 'dart:io';

import 'package:dia/dia.dart';
import 'package:dia_router/dia_router.dart';

class CustomContext extends Context with Routing {
  CustomContext(HttpRequest request) : super(request);
}

void main() {
  final app = App<CustomContext>();

  final router = Router<CustomContext>('/');

  app.use((ctx, next) async {
    ctx.set('Server', 'Dia');
    ctx.set('Date', HttpDate.format(DateTime.now()).toString());
    await next();
  });

  router.get('/plaintext', (ctx, next) async {
    ctx.body = 'Hello, World!';
  });

  router.get('/json', (ctx, next) async {
    ctx.contentType = ContentType.json;
    ctx.body = '{"message":"Hello, World!"}';
  });

  app.use(router.middleware);

  /// Start server listen on localhsot:8080
  app
      .listen('0.0.0.0', 8080)
      .then((info) => print('Server started on http://0.0.0.0:8080'));
}
