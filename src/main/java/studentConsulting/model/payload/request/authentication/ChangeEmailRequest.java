package studentConsulting.model.payload.request.authentication;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ChangeEmailRequest {
    private String oldEmail;
    private String newEmail;
}
