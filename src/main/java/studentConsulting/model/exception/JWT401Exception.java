package studentConsulting.model.exception;

public class JWT401Exception extends RuntimeException {
    private final String message;
    private final String type;
    private final Integer status;

    public JWT401Exception(String message, String type, Integer status) {
        super(message);
        this.message = message;
        this.type = type;
        this.status = status;
    }

    public String getType() {
        return type;
    }

    public Integer getStatus() {
        return status;
    }

    public String getMessage() {
        return message;
    }
}

