package studentConsulting.request.authentication;

import lombok.Data;

@Data
public class loginRequest {
    private String username;

    private String password;
}
