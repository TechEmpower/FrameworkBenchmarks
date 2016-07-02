package hello;

public final class World {
    public static void main(String[] args) throws Exception {
        final Server s = (Server) Class.forName(System.getProperty("serverClass", "hello.JettyServer")).newInstance();
        s.run();
    }

    private World() {}
}
