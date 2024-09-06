package studentConsulting.model.payload.dto;

import java.time.LocalDateTime;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class CommonQuestionDTO {
    private Integer departmentId;
    private Integer fieldId;
    private Integer roleAskId;
    private String title;
    private String content;
    private String fileName; 
    private Integer views;
    private Boolean status;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private String answerContent;
    private String answerUserEmail;  
    private LocalDateTime answerCreatedAt;  
    private String answerTitle;
}