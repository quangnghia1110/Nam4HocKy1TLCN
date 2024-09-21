package studentConsulting.model.payload.request.authentication;

import lombok.Data;

@Data
public class ChangePasswordRequest {

    private String currentPassword;

    private String newPassword;

    private String confirmNewPassword;
}
