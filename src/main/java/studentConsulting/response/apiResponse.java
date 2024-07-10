package studentConsulting.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

import java.util.List;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@JsonInclude(value = JsonInclude.Include.NON_NULL)
public class apiResponse<T> {
    @Builder.Default
    private int status = 200;
    @Builder.Default
    private int error = 0;
    @Builder.Default
    private String message = null;
    private T data;

    public apiResponse(int status) {
        this.status = status;
    }

    public apiResponse(int status, int error) {
        this.status = status;
        this.error = error;
    }

    public apiResponse(T data) {
        this.data = data;
        this.status = 200;
    }
    //Phương thức tĩnh để tạo một đối tượng apiResponse mới từ dữ liệu (data).
    public  static <T> apiResponse<T> of(T data){
        apiResponse apiResponse = new apiResponse<T>();
        apiResponse.setData(data);
        return apiResponse;
    }


}
