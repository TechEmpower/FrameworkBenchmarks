//
//  PlainTextController.swift
//  App
//
//  Created by Gopal Sharma on 7/29/18.
//

import Vapor

final class PlainTextController {

    func get(_ req: Request) throws -> String {
        return "Hello, world!"
    }

}
