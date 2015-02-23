package sabina.benchmark;

/**
 * TODO .
 *
 * @author jam
 */
final class Fortune {
    final int id;
    final String message;

    Fortune (final int id, final String message) {
        this.id = id;
        this.message = message;
    }

    public int getId () { return id; }
    public String getMessage () { return message; }
}
