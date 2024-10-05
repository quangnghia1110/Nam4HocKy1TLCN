package studentConsulting.model.payload.dto.department_field;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ImportFieldDTO {
    private Integer id;
    private LocalDate createdAt;
    private String name;
    private Integer departmentId;
}

