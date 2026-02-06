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
}

private final class HTTPHandler: ChannelInboundHandler {
    public typealias InboundIn = HTTPServerRequestPart
    public typealias OutboundOut = HTTPServerResponsePart

    let jsonEncoder: JSONEncoder
    let dateCache: RFC1123DateCache

    var plaintextBuffer: ByteBuffer
    var jsonBuffer: ByteBuffer

    init(channel: Channel) {
        self.plaintextBuffer = .init(staticString: Constants.plainTextResponse)
        self.jsonBuffer = .init()
        self.jsonEncoder = .init()
        self.dateCache = .on(channel.eventLoop)
    }

    func channelRead(context: ChannelHandlerContext, data: NIOAny) {
        switch self.unwrapInboundIn(data) {
        case .head(let request):
            switch request.uri {
            case "/plaintext":
                let responseHead = self.responseHead(
                    contentType: "text/plain",
                    contentLength: "\(self.plaintextBuffer.readableBytes)"
                )
                self.writeResponse(responseHead, body: self.plaintextBuffer, context: context)
            case "/json":
                let jsonResponse = try! self.jsonEncoder.encode(JSONTestResponse())
                self.jsonBuffer.clear()
                self.jsonBuffer.writeBytes(jsonResponse)
                let responseHead = self.responseHead(
                    contentType: "application/json",
                    contentLength: "\(jsonBuffer.readableBytes)"
                )
                self.writeResponse(responseHead, body: self.jsonBuffer, context: context)
            default:
                context.close(promise: nil)
            }
        case .body, .end:
            break
        }
    }

    func channelReadComplete(context: ChannelHandlerContext) {
        context.flush()
        context.fireChannelReadComplete()
    }

    private func writeResponse(
        _ head: HTTPResponseHead, body: ByteBuffer, context: ChannelHandlerContext
    ) {
        context.write(self.wrapOutboundOut(.head(head)), promise: nil)
        context.write(self.wrapOutboundOut(.body(.byteBuffer(body))), promise: nil)
        context.writeAndFlush(self.wrapOutboundOut(.end(nil)), promise: nil)
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
    .serverChannelOption(
        ChannelOptions.socket(SocketOptionLevel(SOL_SOCKET), SO_REUSEADDR), value: 1
    )
    .childChannelInitializer { channel in
        channel.pipeline.configureHTTPServerPipeline(withPipeliningAssistance: false).flatMap {
            channel.pipeline.addHandler(HTTPHandler(channel: channel))
        }
    }
    .childChannelOption(
        ChannelOptions.socket(SocketOptionLevel(SOL_SOCKET), SO_REUSEADDR), value: 1
    )
    .childChannelOption(ChannelOptions.maxMessagesPerRead, value: 16)

defer {
    try! group.syncShutdownGracefully()
}

let channel = try bootstrap.bind(host: "0.0.0.0", port: 8080).wait()

guard let localAddress = channel.localAddress else {
    fatalError(
        "Address was unable to bind. Please check that the socket was not closed or that the address family was understood."
    )
}

try channel.closeFuture.wait()
