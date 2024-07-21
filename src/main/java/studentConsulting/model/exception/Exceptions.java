package studentConsulting.model.exception;

public class Exceptions {

    /**
     * Ngoại lệ được ném khi không tìm thấy tài nguyên.
     */
    public static class ResourceNotFoundException extends RuntimeException {
        private static final long serialVersionUID = 1L;

        public ResourceNotFoundException(String resourceName, String fieldName, Object fieldValue) {
            super(String.format("%s không tìm thấy với %s: '%s'", resourceName, fieldName, fieldValue));
        }
    }

    /**
     * Ngoại lệ được ném khi token không hợp lệ.
     */
    public static class InvalidTokenException extends RuntimeException {
        private static final long serialVersionUID = 1L;

        public InvalidTokenException(String message) {
            super(message);
        }
    }

    /**
     * Ngoại lệ được ném khi có lỗi xảy ra trong quá trình gửi email.
     */
    public static class EmailSendingException extends RuntimeException {
        private static final long serialVersionUID = 1L;

        public EmailSendingException(String message) {
            super(message);
        }
    }

    /**
     * Ngoại lệ được ném khi tài nguyên đã tồn tại.
     */
    public static class ResourceAlreadyExistsException extends RuntimeException {
        private static final long serialVersionUID = 1L;

        public ResourceAlreadyExistsException(String message) {
            super(message);
        }
    }

    /**
     * Ngoại lệ được ném khi thông tin đăng nhập không hợp lệ.
     */
    public static class InvalidCredentialsException extends RuntimeException {
        private static final long serialVersionUID = 1L;

        public InvalidCredentialsException(String message) {
            super(message);
        }
    }

    /**
     * Ngoại lệ được ném khi mật khẩu không hợp lệ.
     */
    public static class InvalidPasswordException extends RuntimeException {
        private static final long serialVersionUID = 1L;

        public InvalidPasswordException(String message) {
            super(message);
        }
    }

    /**
     * Ngoại lệ được ném khi mã xác thực không hợp lệ.
     */
    public static class InvalidVerifyCodeException extends RuntimeException {
        private static final long serialVersionUID = 1L;

        public InvalidVerifyCodeException(String message) {
            super(message);
        }
    }
}

