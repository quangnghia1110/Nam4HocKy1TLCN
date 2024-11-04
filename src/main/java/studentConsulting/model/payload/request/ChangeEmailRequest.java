package studentConsulting.model.payload.request;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ChangeEmailRequest {
    private String oldEmail;
    private String newEmail;
}
