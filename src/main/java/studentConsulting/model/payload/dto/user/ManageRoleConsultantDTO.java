package studentConsulting.model.payload.dto.user;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;

@Data
@Builder
public class ManageRoleConsultantDTO {
    private Integer id;
    private LocalDate createdAt;
    private String name;
    private Integer roleId;
}
