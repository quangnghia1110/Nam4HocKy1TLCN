package studentConsulting.model.payload.response;

import lombok.Builder;
import lombok.Data;
import studentConsulting.model.entity.authentication.UserInformationEntity;

@Data
@Builder
public class LoginResponse {
    private boolean status;

    private String message;

    private String accessToken;

    private Long expiresIn;

    private String refreshToken;

    private UserInformationEntity userModel;
}
