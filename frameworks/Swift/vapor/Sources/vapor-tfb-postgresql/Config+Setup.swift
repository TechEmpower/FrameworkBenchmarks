import PostgreSQLProvider
import TfbCommon
import LeafProvider

extension Config {
    public func setup() throws {
        // allow fuzzy conversions for these types
        // (add your own types here)
        Node.fuzzy = [Row.self, JSON.self, Node.self]

        try setupProviders()
        try setupPreparations()
        
        self.addConfigurable(middleware: ServerMiddleware(), name: "server")
        self.addConfigurable(middleware: ContentMiddleware(), name: "content-header")
    }
    
    /// Configure providers
    private func setupProviders() throws {
        try addProvider(PostgreSQLProvider.Provider.self)
        try addProvider(LeafProvider.Provider.self)
    }
    
    /// Add all models that should have their
    /// schemas prepared before the app boots
    private func setupPreparations() throws {
        preparations.append(Fortune.self)
        preparations.append(World.self)
    }
}
