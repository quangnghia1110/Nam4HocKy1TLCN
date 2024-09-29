package studentConsulting.model.payload.dto.question_answer;

import java.time.LocalDate;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class DeletionLogDTO {
    private Integer questionId;
    private String questionTitle;
    private String reason;
    private String deletedBy;
    private LocalDate deletedAt;
}

