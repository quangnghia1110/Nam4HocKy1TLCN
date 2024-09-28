package studentConsulting.model.exception;

public class Exceptions {

    public static class ErrorException extends BaseException {
        public ErrorException(String message) {
            super("error", message);
        }
    }

    public static class ErrorExceptionQuestion extends BaseException {
        private static final long serialVersionUID = 1L;
        private final String type;

        public ErrorExceptionQuestion(String message, String type) {
            super("error", message);
            this.type = type;
        }

        public String getType() {
            return type;
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
