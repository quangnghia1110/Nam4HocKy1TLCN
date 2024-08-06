package studentConsulting.model.exception;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import studentConsulting.model.payload.response.ExceptionResponse;

@ControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(Exceptions.BaseException.class)
    public ResponseEntity<ExceptionResponse> handleBaseException(Exceptions.BaseException ex) {
        ExceptionResponse response = new ExceptionResponse(ex.getStatus(), ex.getMessage());
        return ResponseEntity.status(ex.getStatus()).body(response);
    }

    // Xử lý các ngoại lệ khác nếu cần thiết
    @ExceptionHandler(Exception.class)
    public ResponseEntity<ExceptionResponse> handleGenericException(Exception ex) {
        ExceptionResponse response = new ExceptionResponse(500, "Đã xảy ra lỗi hệ thống");
        return ResponseEntity.status(500).body(response);
    }
}
