package studentConsulting.model.payload.dto.manage;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;

@Data
@Builder
public class ManageRoleAskDTO {
    private Integer id;
    private LocalDate createdAt;
    private String name;
    private Integer roleId;
}
