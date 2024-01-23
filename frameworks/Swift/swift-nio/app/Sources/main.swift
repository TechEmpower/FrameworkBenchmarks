import Foundation
import NIO
import NIOHTTP1

struct JSONTestResponse: Encodable {
    let message = "Hello, World!"
}

enum Constants {
    static let httpVersion = HTTPVersion(major: 1, minor: 1)
    static let serverName = "SwiftNIO"

    static let plainTextResponse: StaticString = "Hello, World!"
    static let plainTextResponseLength = plainTextResponse.utf8CodeUnitCount
    static let plainTextResponseLengthString = String(plainTextResponseLength)

    static let jsonResponseLength = try! JSONEncoder().encode(JSONTestResponse()).count
    static let jsonResponseLengthString = String(jsonResponseLength)
}

private final class HTTPHandler: ChannelInboundHandler {
    public typealias InboundIn = HTTPServerRequestPart
    public typealias OutboundOut = HTTPServerResponsePart

    let jsonEncoder: JSONEncoder
    let dateCache: RFC1123DateCache

    var plaintextBuffer: ByteBuffer
    var jsonBuffer: ByteBuffer

    init(channel: Channel) {
        let allocator = ByteBufferAllocator()
        self.plaintextBuffer = allocator.buffer(capacity: Constants.plainTextResponseLength)
        self.plaintextBuffer.writeStaticString(Constants.plainTextResponse)
        self.jsonBuffer = allocator.buffer(capacity: Constants.jsonResponseLength)
        self.jsonEncoder = .init()
        self.dateCache = .on(channel.eventLoop)
    }

    func channelRead(context: ChannelHandlerContext, data: NIOAny) {
        switch self.unwrapInboundIn(data) {
        case .head(let request):
            switch request.uri {
            case "/plaintext":
                self.processPlaintext(context: context)
            case "/json":
                do {
                    try self.processJSON(context: context)
                } catch {
                    context.close(promise: nil)
                }
            default:
                context.close(promise: nil)
            }
        case .body:
            break
        case .end:
            context.write(self.wrapOutboundOut(.end(nil)), promise: nil)
        }
    }

    func channelReadComplete(context: ChannelHandlerContext) {
        context.flush()
        context.fireChannelReadComplete()
    }

    private func processPlaintext(context: ChannelHandlerContext) {
        let responseHead = self.responseHead(contentType: "text/plain", contentLength: Constants.plainTextResponseLengthString)
        context.write(self.wrapOutboundOut(.head(responseHead)), promise: nil)
        context.write(self.wrapOutboundOut(.body(.byteBuffer(self.plaintextBuffer))), promise: nil)
    }

    private func processJSON(context: ChannelHandlerContext) throws {
        let responseHead = self.responseHead(contentType: "application/json", contentLength: Constants.jsonResponseLengthString)
        context.write(self.wrapOutboundOut(.head(responseHead)), promise: nil)
        self.jsonBuffer.clear()
        try self.jsonBuffer.writeBytes(self.jsonEncoder.encode(JSONTestResponse()))
        context.write(self.wrapOutboundOut(.body(.byteBuffer(self.jsonBuffer))), promise: nil)
    }

    private func responseHead(contentType: String, contentLength: String) -> HTTPResponseHead {
        var headers = HTTPHeaders()
        headers.add(name: "content-type", value: contentType)
        headers.add(name: "content-length", value: contentLength)
        headers.add(name: "server", value: Constants.serverName)
        headers.add(name: "date", value: self.dateCache.currentTimestamp())
        return HTTPResponseHead(
            version: Constants.httpVersion,
            status: .ok,
            headers: headers
        )
    }
}

let group = MultiThreadedEventLoopGroup(numberOfThreads: System.coreCount)
let bootstrap = ServerBootstrap(group: group)
    .serverChannelOption(ChannelOptions.backlog, value: 8192)
    .serverChannelOption(ChannelOptions.socket(SocketOptionLevel(SOL_SOCKET), SO_REUSEADDR), value: 1)
    .childChannelInitializer { channel in
        channel.pipeline.configureHTTPServerPipeline(withPipeliningAssistance: false).flatMap {
            channel.pipeline.addHandler(HTTPHandler(channel: channel))
        }
    }
    .childChannelOption(ChannelOptions.socket(SocketOptionLevel(SOL_SOCKET), SO_REUSEADDR), value: 1)
    .childChannelOption(ChannelOptions.maxMessagesPerRead, value: 16)

defer {
    try! group.syncShutdownGracefully()
}

let channel = try bootstrap.bind(host: "0.0.0.0", port: 8080).wait()

guard let localAddress = channel.localAddress else {
    fatalError("Address was unable to bind. Please check that the socket was not closed or that the address family was understood.")
}

try channel.closeFuture.wait()
