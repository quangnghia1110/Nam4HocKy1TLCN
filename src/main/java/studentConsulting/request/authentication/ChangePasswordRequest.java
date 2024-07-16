package studentConsulting.request.authentication;

import lombok.Data;

@Data
public class ChangePasswordRequest {

    private String password;

    private String newPassword;
}
