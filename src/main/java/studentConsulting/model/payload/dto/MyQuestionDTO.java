package studentConsulting.model.payload.dto;

import java.time.LocalDateTime;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class MyQuestionDTO {
    private String title; 
    private String content; 
    private LocalDateTime createdAt; 
    private Integer views; 
    private String fileName;
    private String answerTitle; 
    private String answerContent; 
    private String answerUserEmail; 
    private LocalDateTime answerCreatedAt; 
}
