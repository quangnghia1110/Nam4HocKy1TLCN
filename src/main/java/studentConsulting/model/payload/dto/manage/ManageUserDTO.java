package studentConsulting.model.payload.dto.manage;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ManageUserDTO {
    private Integer id;
    private String avatarUrl;
    private LocalDate createdAt;
    private String firstName;
    private String lastName;
    private String gender;
    private String phone;
    private String schoolName;
    private String studentCode;
    private AddressDTO address;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AddressDTO {
        private String line;
        private String provinceFullName;
        private String districtFullName;
        private String wardFullName;
    }
}
