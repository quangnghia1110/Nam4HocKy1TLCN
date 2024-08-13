package studentConsulting.model.payload.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@JsonInclude(value = JsonInclude.Include.NON_NULL)
public class DataResponse<T> {
    @Builder.Default
    private int status = 200;
    @Builder.Default
    private int error = 0;
    @Builder.Default
    private String message = "";
    private T data;

    public DataResponse(int status) {
        this.status = status;
    }

    public DataResponse(int status, int error) {
        this.status = status;
        this.error = error;
    }

    public DataResponse(T data) {
        this.data = data;
        this.status = 200;
    }

    public DataResponse(int status, String message, T data) {
        this.status = status;
        this.message = message;
        this.data = data;
    }

    // Static method to create a new DataResponse with default values
    public static <T> DataResponse<T> of(int status, String message, T data) {
        return new DataResponse<>(status, message, data);
    }
}
