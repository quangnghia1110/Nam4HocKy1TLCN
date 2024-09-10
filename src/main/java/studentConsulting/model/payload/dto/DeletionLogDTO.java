package studentConsulting.model.payload.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DeletionLogDTO {
	private Integer id;
    private QuestionDTO question;
    private String reason;
    private String deletedBy;
    private LocalDateTime deletedAt;
}
