package studentConsulting.model.payload.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;

@Data
@Builder
public class MyQuestionDTO {
    private Integer id;
    private DepartmentDTO department;
    private FieldDTO field;
    private RoleAskDTO roleAsk;

    // Thông tin của câu hỏi
    private String title;
    private String content;
    private LocalDate createdAt;
    private Integer views;
    private String fileName;
    private String askerFirstname;
    private String askerLastname;

    // Thông tin của câu trả lời
    private String answerTitle;
    private String answerContent;
    private String answerUserEmail;
    private String answerUserFirstname;
    private String answerUserLastname;
    private LocalDate answerCreatedAt;

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
