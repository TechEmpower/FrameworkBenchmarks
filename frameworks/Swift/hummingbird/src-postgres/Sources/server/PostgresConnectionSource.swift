import Hummingbird
import Logging
import PostgresNIO

extension PostgresConnection: HBConnection {
    public func close(on eventLoop: EventLoop) -> EventLoopFuture<Void> {
        return close().hop(to: eventLoop)
    }
}

struct PostgresConnectionSource: HBConnectionSource {
    typealias Connection = PostgresConnection
    
    let configuration: PostgresConnection.Configuration

    init(configuration: PostgresConnection.Configuration) {
        self.configuration = configuration
    }

    func makeConnection(on eventLoop: EventLoop, logger: Logger) -> EventLoopFuture<Connection> {
        let connection = PostgresConnection.connect(on: eventLoop, configuration: self.configuration, id: 0, logger: logger)
        return connection
    }
}

