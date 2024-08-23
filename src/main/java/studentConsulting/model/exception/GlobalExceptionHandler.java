package studentConsulting.model.exception;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import studentConsulting.model.payload.response.ExceptionResponse;

@ControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(Exceptions.BaseException.class)
    public ResponseEntity<ExceptionResponse> handleBaseException(Exceptions.BaseException ex) {
        ExceptionResponse response = ExceptionResponse.builder()
                .status("error") 
                .message(ex.getMessage())
                .build();
        return ResponseEntity.status(ex.getHttpStatus()).body(response);
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ExceptionResponse> handleGenericException(Exception ex) {
        ExceptionResponse response = ExceptionResponse.builder()
                .status("error") // Always set to "error" for exceptions
                .message("Đã xảy ra lỗi hệ thống")
                .build();
        return ResponseEntity.status(500).body(response);
    }
}
