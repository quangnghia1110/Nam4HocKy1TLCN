package studentConsulting.model.payload.dto.actor;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AdvisorSummaryDTO {
    private Integer consultantId;
    private String consultantName;
    private String consultantEmail;

    private long numberOfRatings;
    private double avgGeneralSatisfaction;
    private double avgExpertiseKnowledge;
    private double avgAttitude;
    private double avgResponseSpeed;
    private double avgUnderstanding;

    private double avgOverallScore;
}

