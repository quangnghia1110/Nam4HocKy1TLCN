package studentConsulting.model.payload.response;

import lombok.Builder;
import lombok.Data;
import studentConsulting.model.entity.authentication.UserInformationEntity;

@Data
@Builder
public class RegisterResponse {
    private boolean status;

    private String message;

    private UserInformationEntity userModel;
}
