//
//  FoundationEssentialsJSON+ContentCoders.swift
//  
//
//  Created by Yakov Shapovalov on 04.07.2024.
//

import FoundationPreview
import Vapor

typealias FEJSONEncoder = FoundationEssentials.JSONEncoder
typealias FEJSONDecoder = FoundationEssentials.JSONDecoder

extension FEJSONEncoder {
    /// Convenience for creating a customized ``FoundationEssentials/JSONEncoder``.
    ///
    ///     let encoder: JSONEncoder = .custom(dates: .millisecondsSince1970)
    ///
    /// - Parameters:
    ///   - dates: Date encoding strategy.
    ///   - data: Data encoding strategy.
    ///   - keys: Key encoding strategy.
    ///   - format: Output formatting.
    ///   - floats: Non-conforming float encoding strategy.
    ///   - userInfo: Coder userInfo.
    /// - Returns: Newly created ``FoundationEssentials/JSONEncoder``.
    static func custom(
        dates dateStrategy: FEJSONEncoder.DateEncodingStrategy? = nil,
        data dataStrategy: FEJSONEncoder.DataEncodingStrategy? = nil,
        keys keyStrategy: FEJSONEncoder.KeyEncodingStrategy? = nil,
        format outputFormatting: FEJSONEncoder.OutputFormatting? = nil,
        floats floatStrategy: FEJSONEncoder.NonConformingFloatEncodingStrategy? = nil,
        userInfo: [CodingUserInfoKey: Any]? = nil
    ) -> FEJSONEncoder {
        let json = FEJSONEncoder()
        if let dateStrategy = dateStrategy {
            json.dateEncodingStrategy = dateStrategy
        }
        if let dataStrategy = dataStrategy {
            json.dataEncodingStrategy = dataStrategy
        }
        if let keyStrategy = keyStrategy {
            json.keyEncodingStrategy = keyStrategy
        }
        if let outputFormatting = outputFormatting {
            json.outputFormatting = outputFormatting
        }
        if let floatStrategy = floatStrategy {
            json.nonConformingFloatEncodingStrategy = floatStrategy
        }
        if let userInfo = userInfo {
            json.userInfo = userInfo
        }
        return json
    }
}

extension FEJSONDecoder {
    /// Convenience for creating a customized ``Foundation/JSONDecoder``.
    ///
    ///     let decoder: JSONDecoder = .custom(dates: .millisecondsSince1970)
    ///
    /// - Parameters:
    ///   - dates: Date decoding strategy.
    ///   - data: Data decoding strategy.
    ///   - keys: Key decoding strategy.
    ///   - floats: Non-conforming float decoding strategy.
    ///   - userInfo: Coder userInfo.
    /// - Returns: Newly created ``JSONDecoder``.
    static func custom(
        dates dateStrategy: FEJSONDecoder.DateDecodingStrategy? = nil,
        data dataStrategy: FEJSONDecoder.DataDecodingStrategy? = nil,
        keys keyStrategy: FEJSONDecoder.KeyDecodingStrategy? = nil,
        floats floatStrategy: FEJSONDecoder.NonConformingFloatDecodingStrategy? = nil,
        userInfo: [CodingUserInfoKey: Any]? = nil
    ) -> FEJSONDecoder {
        let json = FEJSONDecoder()
        if let dateStrategy = dateStrategy {
            json.dateDecodingStrategy = dateStrategy
        }
        if let dataStrategy = dataStrategy {
            json.dataDecodingStrategy = dataStrategy
        }
        if let keyStrategy = keyStrategy {
            json.keyDecodingStrategy = keyStrategy
        }
        if let floatStrategy = floatStrategy {
            json.nonConformingFloatDecodingStrategy = floatStrategy
        }
        if let userInfo = userInfo {
            json.userInfo = userInfo
        }
        return json
    }
}

extension FEJSONEncoder: ContentEncoder {
    public func encode<E>(_ encodable: E, to body: inout ByteBuffer, headers: inout HTTPHeaders) throws
    where E: Encodable
    {
        try self.encode(encodable, to: &body, headers: &headers, userInfo: [:])
    }

    public func encode<E>(_ encodable: E, to body: inout ByteBuffer, headers: inout HTTPHeaders, userInfo: [CodingUserInfoKey: Sendable]) throws
    where E: Encodable
    {
        headers.contentType = .json

        if !userInfo.isEmpty { // Changing a coder's userInfo is a thread-unsafe mutation, operate on a copy
            try body.writeBytes(FEJSONEncoder.custom(
                dates: self.dateEncodingStrategy,
                data: self.dataEncodingStrategy,
                keys: self.keyEncodingStrategy,
                format: self.outputFormatting,
                floats: self.nonConformingFloatEncodingStrategy,
                userInfo: self.userInfo.merging(userInfo) { $1 }
            ).encode(encodable))
        } else {
            try body.writeBytes(self.encode(encodable))
        }
    }
}

extension FEJSONDecoder: ContentDecoder {
    public func decode<D>(_ decodable: D.Type, from body: ByteBuffer, headers: HTTPHeaders) throws -> D
    where D: Decodable
    {
        try self.decode(D.self, from: body, headers: headers, userInfo: [:])
    }

    public func decode<D>(_ decodable: D.Type, from body: ByteBuffer, headers: HTTPHeaders, userInfo: [CodingUserInfoKey: Sendable]) throws -> D
    where D: Decodable
    {
        let data = body.getData(at: body.readerIndex, length: body.readableBytes) ?? FEData()

        if !userInfo.isEmpty {
            let actualDecoder = FEJSONDecoder() // Changing a coder's userInfo is a thread-unsafe mutation, operate on a copy
            actualDecoder.dateDecodingStrategy = self.dateDecodingStrategy
            actualDecoder.dataDecodingStrategy = self.dataDecodingStrategy
            actualDecoder.nonConformingFloatDecodingStrategy = self.nonConformingFloatDecodingStrategy
            actualDecoder.keyDecodingStrategy = self.keyDecodingStrategy
#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
            if #available(macOS 12.0, iOS 15.0, tvOS 15.0, watchOS 8.0, *) {
                actualDecoder.allowsJSON5 = self.allowsJSON5
                actualDecoder.assumesTopLevelDictionary = self.assumesTopLevelDictionary
            }
#endif
            actualDecoder.userInfo = self.userInfo.merging(userInfo) { $1 }
            return try actualDecoder.decode(D.self, from: data)
        } else {
            return try self.decode(D.self, from: data)
        }
    }
}

