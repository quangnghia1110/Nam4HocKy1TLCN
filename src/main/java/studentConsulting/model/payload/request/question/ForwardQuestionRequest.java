package studentConsulting.model.payload.request.question;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ForwardQuestionRequest {
    private Integer fromDepartmentId;
    private Integer toDepartmentId;
    private Integer questionId;
    private Integer consultantId;
}

