package studentConsulting.model.exception;

import java.util.List;

public class CustomFieldErrorException extends RuntimeException {
    private final List<FieldErrorDetail> errors;

    public CustomFieldErrorException(List<FieldErrorDetail> errors) {
        this.errors = errors;
    }

    public List<FieldErrorDetail> getErrors() {
        return errors;
    }
}
