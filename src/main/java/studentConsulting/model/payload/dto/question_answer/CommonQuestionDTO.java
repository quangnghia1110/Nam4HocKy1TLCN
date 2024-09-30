package studentConsulting.model.payload.dto.question_answer;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;

@Data
@Builder
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

    private String createdBy;

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
