package studentConsulting.model.payload.response;

import lombok.Builder;
import lombok.Data;
import studentConsulting.model.entity.authentication.UserEntity;

@Data
@Builder
public class LoginResponse {
    private boolean status;

    private String message;

    private String accessToken;

    private Long expiresIn;

    private String refreshToken;

    private UserEntity userModel;
}
