package studentConsulting.model.payload.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;
import studentConsulting.model.payload.dto.UserInformationDTO;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@JsonInclude(value = JsonInclude.Include.NON_NULL)
public class DataResponse<T> {
    @Builder.Default
    private String status = "success";
    @Builder.Default
    private String message = "";
    private T data;

    public DataResponse(String status, String message) {
        this.status = status;
        this.message = message;
    }

    public DataResponse(T data) {
        this.data = data;
    }

    public static <T> DataResponse<T> of(String status, String message, T data) {
        return new DataResponse<>(status, message, data);
    }

    @Data
    @Builder
    public static class LoginData {
        private UserInformationDTO user;
        private String accessToken;
        private Long expiresIn;
        private String refreshToken;
    }
}
