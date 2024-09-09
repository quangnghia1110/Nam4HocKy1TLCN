package studentConsulting.model.payload.dto;

import java.time.LocalDateTime;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class AnswerDTO {
    private Integer questionId;
    private Integer roleConsultantId;
    private Integer userId;
    private String title;
    private String content;
    private String file;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private Boolean statusApproval; 
    private Boolean statusAnswer;
}
