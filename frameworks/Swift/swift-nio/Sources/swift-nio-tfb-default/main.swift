
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
    static let plainTextResponseLength = plainTextResponse.count
    static let plainTextResponseLengthString = String(plainTextResponseLength)

    static let jsonResponseLength = try! JSONEncoder().encode(JSONTestResponse()).count
    static let jsonResponseLengthString = String(jsonResponseLength)
}

private final class HTTPHandler: ChannelInboundHandler {

    public typealias InboundIn = HTTPServerRequestPart
    public typealias OutboundOut = HTTPServerResponsePart

    let dateFormatter = RFC1123DateFormatter()
    let jsonEncoder = JSONEncoder()

    var plaintextBuffer: ByteBuffer!
    var jsonBuffer: ByteBuffer!

    init() {
        let allocator = ByteBufferAllocator()

        plaintextBuffer = allocator.buffer(capacity: Constants.plainTextResponseLength)
        plaintextBuffer.write(staticString: Constants.plainTextResponse)

        jsonBuffer = allocator.buffer(capacity: Constants.jsonResponseLength)
    }

    func channelRead(ctx: ChannelHandlerContext, data: NIOAny) {
        let reqPart = self.unwrapInboundIn(data)

        switch reqPart {
        case .head(let request):
            switch request.uri {
            case "/plaintext":
                processPlaintext(ctx: ctx)
            case "/json":
                processJSON(ctx: ctx)
            default:
                _ = ctx.close()
            }
        case .body:
            break
        case .end:
            _ = ctx.write(self.wrapOutboundOut(.end(nil)))
        }
    }

    func channelReadComplete(ctx: ChannelHandlerContext) {
        ctx.flush()
        ctx.fireChannelReadComplete()
    }

    private func processPlaintext(ctx: ChannelHandlerContext) {
        let responseHead = plainTextResponseHead(contentLength: Constants.plainTextResponseLengthString)
        ctx.write(self.wrapOutboundOut(.head(responseHead)), promise: nil)
        ctx.write(self.wrapOutboundOut(.body(.byteBuffer(plaintextBuffer))), promise: nil)
    }

    private func processJSON(ctx: ChannelHandlerContext) {
        let responseHead = jsonResponseHead(contentLength: Constants.jsonResponseLengthString)
        ctx.write(self.wrapOutboundOut(.head(responseHead)), promise: nil)

        let responseData = try! jsonEncoder.encode(JSONTestResponse())
        jsonBuffer.clear()
        jsonBuffer.write(bytes: responseData)
        ctx.write(self.wrapOutboundOut(.body(.byteBuffer(jsonBuffer))), promise: nil)
    }

    private func jsonResponseHead(contentLength: String) -> HTTPResponseHead {
        return responseHead(contentType: "application/json", contentLength: contentLength)
    }

    private func plainTextResponseHead(contentLength: String) -> HTTPResponseHead {
        return responseHead(contentType: "text/plain", contentLength: contentLength)
    }

    private func responseHead(contentType: String, contentLength: String) -> HTTPResponseHead {
        var headers = HTTPHeaders()
        headers.replaceOrAdd(name: "content-type", value: contentType)
        headers.replaceOrAdd(name: "content-length", value: contentLength)
        headers.replaceOrAdd(name: "server", value: Constants.serverName)
        headers.replaceOrAdd(name: "date", value: dateFormatter.getDate())

        return HTTPResponseHead(version: Constants.httpVersion,
                                status: .ok,
                                headers: headers)
    }
}

let group = MultiThreadedEventLoopGroup(numThreads: System.coreCount)
let bootstrap = ServerBootstrap(group: group)
    .serverChannelOption(ChannelOptions.backlog, value: 8192)
    .serverChannelOption(ChannelOptions.socket(SocketOptionLevel(SOL_SOCKET), SO_REUSEADDR), value: 1)
    .serverChannelOption(ChannelOptions.socket(SocketOptionLevel(SOL_TCP), TCP_NODELAY), value: 1)

    .childChannelInitializer { channel in
        channel.pipeline.configureHTTPServerPipeline(withPipeliningAssistance: false).then {
            channel.pipeline.add(handler: HTTPHandler())
        }
    }

    .childChannelOption(ChannelOptions.socket(SocketOptionLevel(SOL_SOCKET), SO_REUSEADDR), value: 1)
    .childChannelOption(ChannelOptions.maxMessagesPerRead, value: 16)
    .childChannelOption(ChannelOptions.recvAllocator, value: AdaptiveRecvByteBufferAllocator())

defer {
    try! group.syncShutdownGracefully()
}

let channel = try! bootstrap.bind(host: "0.0.0.0", port: 8080).wait()

try! channel.closeFuture.wait()
