import Foundation
import NIOCore
import NIOHTTP1
import NIOPosix

struct JSONTestResponse: Encodable {
    let message = "Hello, World!"
}

private final class HTTPHandler: ChannelInboundHandler {
    public typealias InboundIn = HTTPServerRequestPart
    public typealias OutboundOut = HTTPServerResponsePart

    let jsonEncoder: JSONEncoder
    let dateCache: RFC1123DateCache

    var outputBuffer: ByteBuffer

    init(channel: Channel) {
        self.outputBuffer = .init()
        self.dateCache = .on(channel.eventLoop)
        self.jsonEncoder = JSONEncoder()
    }

    func channelRead(context: ChannelHandlerContext, data: NIOAny) {
        switch self.unwrapInboundIn(data) {
        case .head(let request):
            switch request.uri {
            case "/plaintext":
                self.outputBuffer.clear()
                self.outputBuffer.writeStaticString("Hello, World!")
                let responseHead = self.responseHead(
                    contentType: "text/plain",
                    contentLength: "\(self.outputBuffer.readableBytes)"
                )
                self.writeResponse(responseHead, body: self.outputBuffer, context: context)
            case "/json":
                let jsonResponse = try! self.jsonEncoder.encode(JSONTestResponse())
                self.outputBuffer.clear()
                self.outputBuffer.writeBytes(jsonResponse)
                let responseHead = self.responseHead(
                    contentType: "application/json",
                    contentLength: "\(self.outputBuffer.readableBytes)"
                )
                self.writeResponse(responseHead, body: self.outputBuffer, context: context)
            default:
                context.close(promise: nil)
            }
        case .body, .end:
            break
        }
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
        headers.reserveCapacity(4)
        headers.add(name: "content-type", value: contentType)
        headers.add(name: "content-length", value: contentLength)
        headers.add(name: "server", value: "SwiftNIO")
        headers.add(name: "date", value: self.dateCache.currentTimestamp())
        return HTTPResponseHead(
            version: HTTPVersion(major: 1, minor: 1),
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
