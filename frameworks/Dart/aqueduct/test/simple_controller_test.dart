import 'harness/app.dart';

Future main() async {
  TestApplication app = new TestApplication();

  setUpAll(() async {
    await app.start();
  });

  tearDownAll(() async {
    await app.stop();
  });

  tearDown(() async {
    await app.discardPersistentData();
  });

  group("Success flow", () {
    test("Can create model", () async {
      var request = app.client.request("/model")
        ..json = {
          "name": "Bob"
        };

      var response = await request.post();
      expect(response, hasResponse(200, {
        "id": isNotNull,
        "name": "Bob",
        "createdAt": isTimestamp
      }));
    });

    test("Can get model", () async {
      var request = app.client.request("/model")
        ..json = {
          "name": "Bob"
        };

      var response = await request.post();
      var createdModelID = response.asMap["id"];

      response = await app.client.request("/model/$createdModelID").get();
      expect(response, hasResponse(200, {
        "id": response.asMap["id"],
        "name": response.asMap["name"],
        "createdAt": response.asMap["createdAt"]
      }));
    });
  });
}
