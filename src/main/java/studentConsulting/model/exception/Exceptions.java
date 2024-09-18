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
    public static class JWT401Exception extends BaseException {
        private static final long serialVersionUID = 1L;

        public JWT401Exception(String message) {
            super("401", message); // "401" là mã trạng thái, hoặc bạn có thể thay đổi cho phù hợp
        }
    }

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
