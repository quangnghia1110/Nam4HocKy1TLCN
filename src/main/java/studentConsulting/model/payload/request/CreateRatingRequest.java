package studentConsulting.model.payload.request;

import lombok.Builder;
import lombok.Data;

@Builder
@Data
public class CreateRatingRequest {
    private Integer consultantId;
    private Integer departmentId;
    private int generalSatisfaction;
    private String generalComment;
    private int expertiseKnowledge;
    private String expertiseComment;
    private int attitude;
    private String attitudeComment;
    private int responseSpeed;
    private String responseSpeedComment;
    private int understanding;
    private String understandingComment;
}
