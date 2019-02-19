# Crystal-Onyx

This is the [Onyx](https://github.com/onyxframework/) test of the Framework Benchmarks. Crystal is a new language that closely resembles Ruby with a goal of removing typed variables and parameters (instead inferencing), whilst maintaining top speed through bindings into C.

The framework consists of multiple components:

- Onyx::HTTP 2 is a collection of HTTP handlers, which essentialy are building blocks for your web application
- Onyx::REST 2 is a REST layer on top of Onyx::HTTP which implements splitting business and rendering logic into Actions and Views, inspired by Hanami
- Onyx::SQL 5 is a database-agnostic SQL ORM

Onyx Framework is designed to be both powerful and adoptable by Crystal newcomers. It utilizes complex concepts like annotations and generics, but hides it under beautiful DSL. Such an approach makes it possible to write less code, thus reducing the possibility of bugs, but still make it easy to extend the framework’s functionality.
