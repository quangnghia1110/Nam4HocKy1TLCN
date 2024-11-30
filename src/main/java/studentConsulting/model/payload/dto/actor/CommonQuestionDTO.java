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
    private FieldDTO field;
    private RoleAskDTO roleAsk;

    private String title;
    private String content;
    private String fileName;
    private Integer views;
    private LocalDate createdAt;

    private String askerFirstname;
    private String askerLastname;

    private String answerTitle;
    private String answerContent;
    private String answerUserEmail;
    private String answerUserFirstname;
    private String answerUserLastname;
    private LocalDate answerCreatedAt;

    private Integer createdBy;
    private Boolean status;

    @Data
    @Builder
    public static class DepartmentDTO {
        private Integer id;
        private String name;
    }

    @Data
    @Builder
    public static class FieldDTO {
        private Integer id;
        private String name;
    }

    @Data
    @Builder
    public static class RoleAskDTO {
        private Integer id;
        private String name;
    }
}
