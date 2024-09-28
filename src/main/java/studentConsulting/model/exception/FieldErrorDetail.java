package studentConsulting.model.exception;

public class FieldErrorDetail {
    private final String field;
    private final String message;

    public FieldErrorDetail(String field, String message) {
        this.field = field;
        this.message = message;
    }

    public String getField() {
        return field;
    }

    public String getMessage() {
        return message;
    }
}
