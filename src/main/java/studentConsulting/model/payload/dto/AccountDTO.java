package studentConsulting.model.payload.dto;

import java.sql.Timestamp;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class AccountDTO {
    private Integer id;
    private String email;
    private String username;
    private boolean isActivity;
    private String verifyCode;
    private String verifyRegister;
    private RoleDTO role;
}
