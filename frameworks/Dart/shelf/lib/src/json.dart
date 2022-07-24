import 'dart:convert';
import 'package:shelf/shelf.dart';

const jsonHeader = {'Content-Type': 'application/json'};
const _message = {'message': 'Hello, World!'};

Response json(Request request) => Response.ok(
      jsonEncode(_message),
      headers: jsonHeader,
    );
