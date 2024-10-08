package studentConsulting.model.payload.dto.department_field;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

@Data
@AllArgsConstructor
@Builder
public class DepartmentDTO {
    private Integer id;
    private String name;
}
