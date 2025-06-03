package benchmark;

public class TFBRest extends TFBBase {
    public static void main(String[] args) {
        var server = commonBuilderWithRestHandler().start();
        LOGGER.info("Server started at {}", server.uri());
    }

}
