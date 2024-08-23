package studentConsulting.model.exception;

import org.springframework.http.HttpStatus;
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
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
    }
}
