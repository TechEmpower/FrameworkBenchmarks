// Setup custom JSONEncoder/Decoder

import IkigaJSON
import Vapor


extension IkigaJSONEncoder: ContentEncoder {
    public func encode<E: Encodable>(
        _ encodable: E,
        to body: inout ByteBuffer,
        headers: inout HTTPHeaders
    ) throws {
        headers.contentType = .json
        try self.encodeAndWrite(encodable, into: &body)
    }

    public func encode<E>(_ encodable: E, to body: inout ByteBuffer, headers: inout HTTPHeaders, userInfo: [CodingUserInfoKey : Sendable]) throws where E : Encodable {
        var encoder = self
        encoder.userInfo = userInfo
        headers.contentType = .json
        try encoder.encodeAndWrite(encodable, into: &body)
    }

    public func encode<E>(_ encodable: E, to body: inout ByteBuffer, headers: inout HTTPHeaders, userInfo: [CodingUserInfoKey : Any]) throws where E : Encodable {
        var encoder = self
        encoder.userInfo = userInfo
        headers.contentType = .json
        try encoder.encodeAndWrite(encodable, into: &body)
    }
}

extension IkigaJSONDecoder: ContentDecoder {
    public func decode<D: Decodable>(
        _ decodable: D.Type,
        from body: ByteBuffer,
        headers: HTTPHeaders
    ) throws -> D {
        return try self.decode(D.self, from: body)
    }
    
    public func decode<D>(_ decodable: D.Type, from body: ByteBuffer, headers: HTTPHeaders, userInfo: [CodingUserInfoKey : Sendable]) throws -> D where D : Decodable {
        let decoder = IkigaJSONDecoder(settings: settings)
        decoder.settings.userInfo = userInfo
        return try decoder.decode(D.self, from: body)
    }

    public func decode<D>(_ decodable: D.Type, from body: ByteBuffer, headers: HTTPHeaders, userInfo: [CodingUserInfoKey : Any]) throws -> D where D : Decodable {
        let decoder = IkigaJSONDecoder(settings: settings)
        decoder.settings.userInfo = userInfo
        return try decoder.decode(D.self, from: body)
    }
}
