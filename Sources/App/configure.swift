import IkigaJSON
import PostgresBridge
import Vapor
import VaporBridges


extension DatabaseHost {
    public static var DbHost: DatabaseHost {
        return .init(
            hostname: "tfb-database",
            port: 5432,
            username: "benchmarkdbuser",
            password: "benchmarkdbpassword",
            tlsConfiguration: nil
        )
    }
}

extension DatabaseIdentifier {
    public static var Db: DatabaseIdentifier {
        .init(name: "hello_world", host: .DbHost)
    }
}


public func configure(_ app: Application) throws {
    let decoder = IkigaJSONDecoder()
    decoder.settings.dateDecodingStrategy = .iso8601
    ContentConfiguration.global.use(decoder: decoder as ContentDecoder, for: .json)

    var encoder = IkigaJSONEncoder()
    encoder.settings.dateEncodingStrategy = .iso8601
    ContentConfiguration.global.use(encoder: encoder as ContentEncoder, for: .json)

    app.http.server.configuration.serverName = "Vapor"
    app.logger.logLevel = .notice
    app.logger.notice("💧 VAPOR")
    app.logger.notice("System.coreCount: \(System.coreCount)")
    
    try routes(app)
    
    try app.run()
}
