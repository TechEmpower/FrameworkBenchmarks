package hello;

import io.netty.bootstrap.ServerBootstrap;
import io.netty.buffer.PooledByteBufAllocator;
import io.netty.channel.Channel;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.ServerChannel;
import io.netty.channel.epoll.EpollEventLoopGroup;
import io.netty.channel.epoll.EpollServerSocketChannel;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.util.ResourceLeakDetector;
import io.netty.util.ResourceLeakDetector.Level;


public class HelloWebServer {
    static {
        ResourceLeakDetector.setLevel(Level.DISABLED);
    }

    private final int port;

    public HelloWebServer(int port) {
        this.port = port;
    }

    public void run() throws Exception {
		// Configure the server.

		String os = System.getProperty("os.name").toLowerCase();

		boolean is_linux = os.contains("linux");
		
		if (is_linux) {
			doRun( new EpollEventLoopGroup(), EpollServerSocketChannel.class);
		}
		else {
			doRun(new NioEventLoopGroup(), NioServerSocketChannel.class);
		} 
    }
    
	private void doRun(EventLoopGroup loupGroup, Class<? extends ServerChannel> serverChannelClass) throws InterruptedException	{
		try {
			ServerBootstrap b = new ServerBootstrap();
			b.option(ChannelOption.SO_BACKLOG, 1024);
			b.option(ChannelOption.SO_REUSEADDR, true);
			b.group(loupGroup).channel(serverChannelClass).childHandler(new HelloServerInitializer());			
			b.childOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT);

			Channel ch = b.bind(port).sync().channel();
			ch.closeFuture().sync();
		}
		finally {
			loupGroup.shutdownGracefully().sync();
		}
	}

    public static void main(String[] args) throws Exception {
        int port;
        if (args.length > 0) {
            port = Integer.parseInt(args[0]);
        } else {
            port = 8080;
        }
        new HelloWebServer(port).run();
    }
}
