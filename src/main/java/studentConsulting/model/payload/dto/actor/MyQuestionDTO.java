package studentConsulting.model.payload.dto.actor;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
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
    private String askerId;

    // Thông tin của câu trả lời
    private Integer answerId;
    private String answerTitle;
    private String answerContent;
    private String answerUserEmail;
    private String answerUserFirstname;
    private String answerUserLastname;
    private LocalDate answerCreatedAt;
    private String answerAvatarUrl;
    private String answerFileName;

    private List<String> filterStatus;
    private List<MyQuestionDTO> followUpQuestions;

    private ForwardQuestionDTO forwardQuestionDTO;


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
