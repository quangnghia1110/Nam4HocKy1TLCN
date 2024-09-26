package studentConsulting.model.exception;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import studentConsulting.model.payload.response.ExceptionResponse;

import java.util.HashMap;
import java.util.Map;

@ControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(Exceptions.BaseException.class)
    public ResponseEntity<ExceptionResponse> handleBaseException(Exceptions.BaseException ex) {
        ExceptionResponse response = ExceptionResponse.builder()
                .status("error")
                .message(ex.getMessage())
                .build();
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
    }

    @ResponseStatus(HttpStatus.UNPROCESSABLE_ENTITY)
    @ExceptionHandler(CustomFieldErrorException.class)
    public ResponseEntity<Map<String, Object>> handleCustomFieldErrorException(CustomFieldErrorException ex) {
        Map<String, Object> response = new HashMap<>();
        response.put("status", "Error");
        response.put("message", "Validation failed");
        response.put("data", ex.getErrors());

        return new ResponseEntity<>(response, HttpStatus.UNPROCESSABLE_ENTITY);
    }

    @ExceptionHandler(Exceptions.JWT401Exception.class)
    public ResponseEntity<Object> handleJWT401Exception(Exceptions.JWT401Exception ex) {
        Map<String, Object> response = new HashMap<>();
        response.put("status", ex.getStatus());
        response.put("message", ex.getMessage());
        response.put("type", ex.getType());

        return new ResponseEntity<>(response, HttpStatus.UNAUTHORIZED);
    }
}
