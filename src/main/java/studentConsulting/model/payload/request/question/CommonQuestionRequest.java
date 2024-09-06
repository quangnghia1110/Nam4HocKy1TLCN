package studentConsulting.model.payload.request.question;


import java.time.LocalDateTime;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class CommonQuestionRequest {
    private Integer departmentId;
    private Integer fieldId;
    private Integer roleAskId;
    private String title;
    private String content;
    private String fileName; 
    private Integer views;
    private String contentAnswer;
    private Boolean status;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}

