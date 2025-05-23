package benchmark;

public class TFBPg extends TFBBase {
    public static void main(String[] args) {
        var server = commonBuilderWithMuHandler().start();
        LOGGER.info("Server started at {}", server.uri());
    }
}
