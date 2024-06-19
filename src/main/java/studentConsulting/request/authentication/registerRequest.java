package studentConsulting.request.authentication;

import lombok.Builder;
import lombok.Data;

@Data
@Builder

public class registerRequest {
    private String userName;
    private String passWord;
    private String firstname;
    private String lastname;
    private String email;
    private String phone;
    private String occupation;
    private String roleName;
}
