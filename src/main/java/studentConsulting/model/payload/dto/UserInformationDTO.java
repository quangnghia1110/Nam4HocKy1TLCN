package studentConsulting.model.payload.dto;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class UserInformationDTO {
    private Integer id;
    private String username;
    private String studentCode;
    private String schoolName;
    private String firstName;
    private String lastName;
    private String phone;
    private String avatarUrl;
    private String gender;
    private String email;
    private AddressDTO address;
    private AccountDTO account;
}
