package studentConsulting.request.authentication;

import lombok.Data;

@Data
public class resetPasswordRequest {
    private String email;
    private String newPassword;
}
