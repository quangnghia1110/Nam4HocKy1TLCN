package studentConsulting.request.authentication;

import lombok.Data;

@Data
public class LoginRequest {
    private String username;

    private String password;
}
