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
public class ManageDepartmentDTO {
    private Integer id;
    private LocalDate createdAt;
    private String name;
    private String description;
    private String logo;
}
