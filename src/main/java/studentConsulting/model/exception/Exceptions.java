package studentConsulting.model.exception;

public class Exceptions {

    public static class ResourceNotFoundException extends BaseException {
        private static final long serialVersionUID = 1L;

        public ResourceNotFoundException(String resourceName, String fieldName, Object fieldValue) {
            super(404, String.format("%s không tìm thấy với %s: '%s'", resourceName, fieldName, fieldValue));
        }
    }

    public static class InvalidTokenException extends BaseException {
        private static final long serialVersionUID = 1L;

        public InvalidTokenException(String message) {
            super(401, message);
        }
    }

    public static class EmailSendingException extends BaseException {
        private static final long serialVersionUID = 1L;

        public EmailSendingException(String message) {
            super(500, message);
        }
    }

    public static class ResourceAlreadyExistsException extends BaseException {
        private static final long serialVersionUID = 1L;

        public ResourceAlreadyExistsException(String message) {
            super(409, message);
        }
    }

    public static class InvalidCredentialsException extends BaseException {
        private static final long serialVersionUID = 1L;

        public InvalidCredentialsException(String message) {
            super(401, message);
        }
    }

    public static class InvalidPasswordException extends BaseException {
        private static final long serialVersionUID = 1L;

        public InvalidPasswordException(String message) {
            super(400, message);
        }
    }

    public static class InvalidVerifyCodeException extends BaseException {
        private static final long serialVersionUID = 1L;

        public InvalidVerifyCodeException(String message) {
            super(400, message);
        }
    }
    
    public class UnauthorizedAccessException extends RuntimeException {
        private static final long serialVersionUID = 1L;

        public UnauthorizedAccessException(String message) {
            super(message);
        }
    }



    
    public abstract static class BaseException extends RuntimeException {
        private static final long serialVersionUID = 1L;
        private final int status;

        public BaseException(int status, String message) {
            super(message);
            this.status = status;
        }

        public int getStatus() {
            return status;
        }
    }
}
