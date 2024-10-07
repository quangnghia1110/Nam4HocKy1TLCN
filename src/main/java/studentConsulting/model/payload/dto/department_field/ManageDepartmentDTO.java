package studentConsulting.model.payload.dto.department_field;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;

@Data
@Builder
public class ManageDepartmentDTO {
    private Integer id;
    private LocalDate createdAt;
    private String name;
    private String description;
    private String logo;
}
