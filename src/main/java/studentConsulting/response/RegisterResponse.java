package studentConsulting.response;

import lombok.Builder;
import lombok.Data;
import studentConsulting.entity.authentication.UserEntity;

@Data
@Builder
public class RegisterResponse {
    private boolean status;

    private String message;

    private UserEntity userModel;
}
