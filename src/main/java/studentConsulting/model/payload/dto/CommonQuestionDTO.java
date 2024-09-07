package studentConsulting.model.payload.dto;

import java.time.LocalDateTime;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class CommonQuestionDTO {
	private DepartmentDTO department;
    private FieldDTO field;
    private RoleAskDTO roleAsk;

    private String title;
    private String content;
    private String fileName; 
    private Integer views;
    private LocalDateTime createdAt;

    // Thông tin của người hỏi
    private String askerFirstname; 
    private String askerLastname; 

    // Thông tin câu trả lời
    private String answerTitle;
    private String answerContent;
    private String answerUserEmail;
    private String answerUserFirstname; 
    private String answerUserLastname;
    private LocalDateTime answerCreatedAt;
    
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
