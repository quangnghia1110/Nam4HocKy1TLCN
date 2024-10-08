package studentConsulting.model.payload.dto.question_answer;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

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
        private String firstName;
        private String lastName;

        public String getFullName() {
            return firstName + " " + lastName;
        }
    }

}

