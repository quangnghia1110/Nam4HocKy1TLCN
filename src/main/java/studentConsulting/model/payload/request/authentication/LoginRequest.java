package studentConsulting.model.payload.request.authentication;

import lombok.Data;

@Data
public class LoginRequest {
    private String email;
    private String password;
}
