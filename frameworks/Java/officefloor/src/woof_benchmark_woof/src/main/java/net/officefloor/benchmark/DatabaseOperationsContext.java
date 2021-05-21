package net.officefloor.benchmark;

import java.util.List;

import net.officefloor.server.http.HttpResponse;
import net.officefloor.server.http.ServerHttpConnection;

/**
 * @author Daniel Sagenschneider
 */
public interface DatabaseOperationsContext {

    /**
     * Obtains {@link Exception} for transient overload of resource.
     *
     * @return {@link Exception} for transient overload of resource.
     */
    Exception getTransientResourceException();

    /**
     * Sends db response.
     *
     * @param response   {@link HttpResponse}.
     * @param connection {@link ServerHttpConnection}.
     * @param world      {@link World} to send.
     */
    void dbSend(HttpResponse response, ServerHttpConnection connection, World world);

    /**
     * Sends queries response.
     *
     * @param response   {@link HttpResponse}.
     * @param connection {@link ServerHttpConnection}.
     * @param worlds     {@link World} instances to send.
     */
    void queriesSend(HttpResponse response, ServerHttpConnection connection, List<World> worlds);

    /**
     * Sends fortunes response.
     *
     * @param response   {@link HttpResponse}.
     * @param connection {@link ServerHttpConnection}.
     * @param fortune    {@link Fortune} instances to send.
     */
    void fortunesSend(HttpResponse response, ServerHttpConnection connection, List<Fortune> fortunes);

    /**
     * Sends update response.
     *
     * @param response   {@link HttpResponse}.
     * @param connection {@link ServerHttpConnection}.
     * @param worlds     {@link World} instances to send.
     */
    void updateSend(HttpResponse response, ServerHttpConnection connection, List<World> worlds);

    /**
     * Sends error.
     *
     * @param connection {@link ServerHttpConnection}.
     * @param failure    Cause.
     */
    void sendError(ServerHttpConnection connection, Throwable failure);

    /**
     * Sends error.
     *
     * @param connection {@link ServerHttpConnection}.
     * @param satus      Status.
     */
    void sendError(ServerHttpConnection connection, int status);

}
