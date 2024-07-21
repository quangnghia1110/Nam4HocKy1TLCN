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
    private String message = null;
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

    // Phương thức tĩnh để tạo một đối tượng apiResponse mới từ dữ liệu (data).
    public static <T> DataResponse<T> of(T data) {
        DataResponse<T> dataResponse = new DataResponse<>(data);
        dataResponse.setData(data);
        return dataResponse;
    }
}
