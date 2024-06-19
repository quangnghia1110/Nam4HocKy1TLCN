package studentConsulting.response;

import lombok.Builder;
import lombok.Data;
import studentConsulting.entity.authentication.userEntity;

@Data
@Builder
public class registerResponse {
    private boolean status;

    private String message;

    private userEntity userModel;
}
