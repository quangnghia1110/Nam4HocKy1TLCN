package studentConsulting.model.payload.dto.question_answer;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.constant.enums.QuestionFilterStatus;

import java.time.LocalDate;
import java.util.List;

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
    private String askerAvatarUrl;

    // Thông tin của câu trả lời
    private String answerTitle;
    private String answerContent;
    private String answerUserEmail;
    private String answerUserFirstname;
    private String answerUserLastname;
    private LocalDate answerCreatedAt;
    private String answerAvatarUrl;

    private String filterStatus;
    private List<MyQuestionDTO> followUpQuestions;

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
    public static class FieldDTO {
        private Integer id;
        private String name;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RoleAskDTO {
        private Integer id;
        private String name;
    }
}
