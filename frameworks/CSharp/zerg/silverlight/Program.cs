using silverlight;
using zerg.Engine;
using zerg.Engine.Configs;

// dotnet publish -f net10.0 -c Release /p:PublishAot=true /p:OptimizationPreference=Speed

var reactorConfigs = new ReactorConfig[Environment.ProcessorCount];
var reactorConfig = new ReactorConfig
{
    RingFlags = 0x3000, // IORING_SETUP_SINGLE_ISSUER | IORING_SETUP_DEFER_TASKRUN
    RingEntries = 1024,
    RecvBufferSize = 1024 * 4,
    BufferRingEntries =  1024 * 16,
    BatchCqes = 1024 * 4,
    MaxConnectionsPerReactor = 1024
};
Array.Fill(reactorConfigs, reactorConfig);

var acceptorConfig = new AcceptorConfig
{
    RingFlags = 0,
    RingEntries = 256,
    BatchSqes = 1024 * 4,
};

var engine = new Engine(new EngineOptions
{
    Ip = "0.0.0.0",
    Port = 8080,
    ReactorCount = Environment.ProcessorCount / 2,
    AcceptorConfig =  acceptorConfig,
    ReactorConfigs = reactorConfigs,
});
engine.Listen();

while (engine.ServerRunning)
{
    var connection = await engine.AcceptAsync();
    if (connection is null) continue;
    _ = new ConnectionHandler().HandleConnectionAsync(connection);
}