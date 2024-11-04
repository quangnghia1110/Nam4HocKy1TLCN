package studentConsulting.model.payload.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UpdateForwardQuestionRequest {
    private Integer toDepartmentId;
    private Integer questionId;
    private Integer consultantId;
    private String title;
}


