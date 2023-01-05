import 'package:angel3_framework/angel3_framework.dart';
import 'package:angel3_test/angel3_test.dart';
import 'package:test/test.dart';
import 'package:orm_mysql_app/orm_mysql_app.dart';

// Angel also includes facilities to make testing easier.
//
// `package:angel_test` ships a client that can test
// both plain HTTP and WebSockets.
//
// Tests do not require your server to actually be mounted on a port,
// so they will run faster than they would in other frameworks, where you
// would have to first bind a socket, and then account for network latency.
//
// See the documentation here:
// https://github.com/angel-dart/test
//
// If you are unfamiliar with Dart's advanced testing library, you can read up
// here:
// https://github.com/dart-lang/test

void main() async {
  late TestClient client;

  setUp(() async {
    var app = Angel();
    await app.configure(configureServer);

    client = await connectTo(app);
  });

  tearDown(() async {
    await client.close();
  });

  test('index returns 200', () async {
    // Request a resource at the given path.
    var response = await client.get(Uri.parse('/'));

    // Expect a 200 response.
    expect(response, hasStatus(200));
  });
}
