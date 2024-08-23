package studentConsulting.model.exception;

public class Exceptions {

    // Exception for resource not found
    public static class ResourceNotFoundException extends BaseException {
        private static final long serialVersionUID = 1L;

        public ResourceNotFoundException(String resourceName, String fieldName, Object fieldValue) {
            super("error", 404, String.format("%s không tìm thấy với %s: '%s'", resourceName, fieldName, fieldValue));
        }
    }

    // Exception for invalid token
    public static class InvalidTokenException extends BaseException {
        private static final long serialVersionUID = 1L;

        public InvalidTokenException(String message) {
            super("error", 401, message);
        }
    }

    // Exception for email sending failures
    public static class EmailSendingException extends BaseException {
        private static final long serialVersionUID = 1L;

        public EmailSendingException(String message) {
            super("error", 500, message);
        }
    }

    // Exception for resource already exists
    public static class ResourceAlreadyExistsException extends BaseException {
        private static final long serialVersionUID = 1L;

        public ResourceAlreadyExistsException(String message) {
            super("error", 409, message);
        }
    }

    // Exception for invalid credentials
    public static class InvalidCredentialsException extends BaseException {
        private static final long serialVersionUID = 1L;

        public InvalidCredentialsException(String message) {
            super("error", 401, message);
        }
    }

    // Exception for invalid password
    public static class InvalidPasswordException extends BaseException {
        private static final long serialVersionUID = 1L;

        public InvalidPasswordException(String message) {
            super("error", 400, message);
        }
    }

    // Exception for invalid verification code
    public static class InvalidVerifyCodeException extends BaseException {
        private static final long serialVersionUID = 1L;

        public InvalidVerifyCodeException(String message) {
            super("error", 400, message);
        }
    }

    // Exception for unauthorized access
    public static class UnauthorizedAccessException extends BaseException {
        private static final long serialVersionUID = 1L;

        public UnauthorizedAccessException(String message) {
            super("error", 403, message);  // 403 Forbidden for unauthorized access
        }
    }

    // Base exception class
    public abstract static class BaseException extends RuntimeException {
        private static final long serialVersionUID = 1L;
        private final String status;
        private final int httpStatus;

        public BaseException(String status, int httpStatus, String message) {
            super(message);
            this.status = status;
            this.httpStatus = httpStatus;
        }

        public String getStatus() {
            return status;
        }

        public int getHttpStatus() {
            return httpStatus;
        }
    }
}
