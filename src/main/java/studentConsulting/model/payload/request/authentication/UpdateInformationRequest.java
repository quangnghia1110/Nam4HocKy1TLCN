package studentConsulting.model.payload.request.authentication;

import lombok.Data;

@Data
public class UpdateInformationRequest {
    private String studentCode;
    private String schoolName;
    private String firstName;
    private String lastName;
    private String phone;
    private String avatarUrl;
    private String gender;
}
