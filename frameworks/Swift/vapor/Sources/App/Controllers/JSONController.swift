//
//  JSONController.swift
//  App
//
//  Created by Gopal Sharma on 7/29/18.
//

import Vapor

final class JSONController {

    func get(_ req: Request) throws -> Message {
        return Message(message: "Hello, world!")
    }

}
