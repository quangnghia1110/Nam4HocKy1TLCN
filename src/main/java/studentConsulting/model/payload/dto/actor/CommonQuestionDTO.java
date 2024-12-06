package studentConsulting.model.payload.dto.actor;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CommonQuestionDTO {

    private Integer commonQuestionId;
    private DepartmentDTO department;
    private String title;
    private String content;
    private String answerTitle;
    private String answerContent;
    private String file;
    private String fileAnswer;
    private Boolean status;
    private CreatedByDTO createdBy;
    private LocalDate createdAt;

    @Data
    @Builder
    public static class DepartmentDTO {
        private Integer id;
        private String name;
    }

    @Data
    @Builder
    public static class CreatedByDTO {
        private Integer id;
        private String name;
    }
}

