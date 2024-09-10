package studentConsulting.model.payload.dto;

import java.time.LocalDateTime;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class DeletionLogDTO {
    private Integer questionId;
    private String questionTitle;
    private String reason;
    private String deletedBy;
    private LocalDateTime deletedAt;
}

