package studentConsulting.model.exception;

public class Exceptions {

    // Exception for resource not found
    public static class ResourceNotFoundException extends BaseException {
        private static final long serialVersionUID = 1L;

        public ResourceNotFoundException(String resourceName, String fieldName, Object fieldValue) {
            super("error", String.format("%s không tìm thấy với %s: '%s'", resourceName, fieldName, fieldValue));
        }
    }
    public static class ErrorException extends BaseException {
        public ErrorException(String message) {
            super("error", message);
        }
    }
  

    // Base exception class
    public abstract static class BaseException extends RuntimeException {
        private static final long serialVersionUID = 1L;
        private final String status;

        public BaseException(String status, String message) {
            super(message);
            this.status = status;
        }

        public String getStatus() {
            return status;
        }
    }
}
