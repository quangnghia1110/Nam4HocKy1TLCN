package studentConsulting.model.payload.dto.user;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.payload.dto.address.AddressDTO;
import studentConsulting.model.payload.dto.authentication.AccountDTO;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserInformationDTO {
    private Integer id;
    private String username;
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
