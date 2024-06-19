package studentConsulting.response;

import lombok.Builder;
import lombok.Data;
import studentConsulting.entity.authentication.userEntity;

@Data
@Builder
public class loginResponse {
    private boolean status;

    private String message;

    private String accessToken;

    private Long expiresIn;

    private String refreshToken;

    private userEntity userModel;
}
