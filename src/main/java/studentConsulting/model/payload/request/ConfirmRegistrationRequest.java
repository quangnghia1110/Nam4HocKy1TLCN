package studentConsulting.model.payload.request;

import lombok.Data;

@Data
public class ConfirmRegistrationRequest {
    private String emailRequest;
    private String token;
}
