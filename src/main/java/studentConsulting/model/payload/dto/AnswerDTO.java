package studentConsulting.model.payload.dto;

import java.time.LocalDate;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class AnswerDTO {
	private Integer answerId;
    private Integer questionId;
    private Integer roleConsultantId;
    private Integer userId;
    private String title;
    private String content;
    private String file;
    private LocalDate createdAt;
    private Boolean statusApproval; 
    private Boolean statusAnswer;
}
