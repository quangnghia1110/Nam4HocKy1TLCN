package studentConsulting.model.payload.dto.department_field;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ImportDepartmentDTO {
    private Integer id;
    private LocalDate createdAt;
    private String description;
    private String logo;
    private String name;
}

