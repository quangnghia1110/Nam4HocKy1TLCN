package studentConsulting.model.payload.dto.manage;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ManageFieldDTO {
    private Integer id;
    private LocalDate createdAt;
    private String name;
    private Integer departmentId;
}

