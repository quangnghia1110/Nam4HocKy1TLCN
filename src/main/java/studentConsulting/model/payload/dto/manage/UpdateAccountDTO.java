package studentConsulting.model.payload.dto.manage;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UpdateAccountDTO {
    private Boolean activity;
    private Integer roleId;
    private Integer roleConsultantId;
    private String username;
    private String email;
    private String password;
}


