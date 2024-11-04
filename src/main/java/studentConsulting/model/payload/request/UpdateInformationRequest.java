package studentConsulting.model.payload.request;

import lombok.Builder;
import lombok.Data;
import studentConsulting.model.payload.dto.actor.AddressDTO;

@Data
@Builder
public class UpdateInformationRequest {
    private String username;
    private String studentCode;
    private String schoolName;
    private String firstName;
    private String lastName;
    private String phone;
    private String avatarUrl;
    private String gender;
    private AddressDTO address;
    private String email;
}
