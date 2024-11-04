package studentConsulting.model.payload.request;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class RegisterRequest {
    private String username;
    private String email;
    private String password;
    private String confirmPassword;
    private String phone;
    private String gender;
}
