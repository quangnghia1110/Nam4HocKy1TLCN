package studentConsulting.model.payload.dto.actor;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
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
