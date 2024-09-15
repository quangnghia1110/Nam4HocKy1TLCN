package studentConsulting.model.exception;

import java.util.HashMap;
import java.util.Map;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;

import studentConsulting.model.payload.response.ExceptionResponse;

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
    
    
}
