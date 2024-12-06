package studentConsulting.model.payload.request;

import lombok.Data;

@Data
public class ResetPasswordRequest {
    private String email;
    private String newPassword;
    private String repeatPassword;
    private String token;
}
