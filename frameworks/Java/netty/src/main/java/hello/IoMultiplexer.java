package hello;

public enum IoMultiplexer {
	EPOLL, KQUEUE, JDK, IO_URING
}