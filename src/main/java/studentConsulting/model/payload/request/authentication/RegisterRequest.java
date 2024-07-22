package studentConsulting.model.payload.request.authentication;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class RegisterRequest {
    private String username;
    private String email;
    private String password;
    private String studentCode;
    private String schoolName;
    private String firstname;
    private String lastname;
    private String phone;
    private String avatarUrl;
    private String gender;
    private String provinceCode;  
    private String districtCode;  
    private String wardCode;   
    private String departmentName;
}
