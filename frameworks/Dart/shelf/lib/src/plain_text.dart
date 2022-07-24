import 'package:shelf/shelf.dart';

const jsonHeader = {'Content-Type': 'text/json'};
const _message = 'Hello, World!';

Response plainText(Request req) => Response.ok(
      _message,
      headers: jsonHeader,
    );
