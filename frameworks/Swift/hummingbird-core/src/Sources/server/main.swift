import Foundation
import HummingbirdCore
import NIOCore
import NIOHTTP1
import NIOPosix

struct TechEmpowerResponder: HBHTTPResponder {
    let plainTextBody = "Hello, world!"

    func respond(to request: HBHTTPRequest, context: ChannelHandlerContext, onComplete: @escaping (Result<HBHTTPResponse, Error>) -> Void) {
        let body: ByteBuffer
        let status: HTTPResponseStatus
        let headers: HTTPHeaders
        switch (request.head.uri, request.head.method) {
        case ("/plaintext", .GET):
            status = .ok
            headers = HTTPHeaders([
                ("content-type", "text/plain"),
                ("date", HBDateCache.getDateCache(on: context.eventLoop).currentDate)
            ])
            body = context.channel.allocator.buffer(string: plainTextBody)

        case ("/json", .GET):
            status = .ok
            headers = HTTPHeaders([
                ("content-type", "application/json"),
                ("date", HBDateCache.getDateCache(on: context.eventLoop).currentDate)
            ])
            let json = try! JSONEncoder().encode(["message": plainTextBody])
            body = context.channel.allocator.buffer(bytes: json)

        default:
            onComplete(.failure(HBHTTPError(.badRequest)))
            return
        }
        let responseHead = HTTPResponseHead(version: .init(major: 1, minor: 1), status: status, headers: headers)
        let response = HBHTTPResponse(head: responseHead, body: .byteBuffer(body))
        onComplete(.success(response))
    }
}

func runApp() throws {
    let eventLoopGroup = MultiThreadedEventLoopGroup(numberOfThreads: System.coreCount)
    defer { try? eventLoopGroup.syncShutdownGracefully() }

    let serverHostName = ProcessInfo.processInfo.environment["SERVER_HOSTNAME"] ?? "127.0.0.1"
    let serverPort = ProcessInfo.processInfo.environment["SERVER_PORT"].map { Int($0) ?? 8080 } ?? 8080
    let configuration = HBHTTPServer.Configuration(
        address: .hostname(serverHostName, port: serverPort),
        serverName: "hb-core",
        backlog: 8192,
        withPipeliningAssistance: false
    )

    let server = HBHTTPServer(group: eventLoopGroup, configuration: configuration)
    try server.start(responder: TechEmpowerResponder()).wait()
    try server.wait()
}

try runApp()
