package benchmark.model;

public class Fortune {

    private int id;
    private String message;

    public Fortune() {}

    public Fortune(int id, String message) {
        this.id = id;
        this.message = message;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public String toString() {
        return "Fortune{" +
                "id=" + id +
                ", message='" + message + '\'' +
                '}';
    }
}
