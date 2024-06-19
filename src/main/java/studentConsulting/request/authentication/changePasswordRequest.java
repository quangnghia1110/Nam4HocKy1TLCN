package studentConsulting.request.authentication;

import lombok.Data;

@Data
public class changePasswordRequest {

    private String password;

    private String newPassword;
}
