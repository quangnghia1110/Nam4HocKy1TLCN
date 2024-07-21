package studentConsulting.model.payload.request.authentication;

import lombok.Data;

@Data
public class ForgotPasswordRequest {
    private String emailRequest;
}
