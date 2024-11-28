package studentConsulting.model.payload.dto.actor;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ForwardQuestionDTO {
    private Integer id;
    private String title;
    private DepartmentDTO fromDepartment;
    private DepartmentDTO toDepartment;
    private ConsultantDTO consultant;
    private Boolean statusForward;
    private Integer createdBy;
    private Integer questionId;
    private LocalDate createdAt;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class DepartmentDTO {
        private Integer id;
        private String name;
    }


    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ConsultantDTO {
        private Integer id;
        private String name;
    }

}

