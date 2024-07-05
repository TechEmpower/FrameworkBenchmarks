//
//  Models+Content.swift
//  
//
//  Created by Yakov Shapovalov on 04.07.2024.
//

import Vapor

extension World: Content {}
extension Fortune: Content {}

struct Fortunes: Codable {
    var fortunes: [Fortune]

    init(_ fortunes: [Fortune]) {
        self.fortunes = fortunes
    }
}