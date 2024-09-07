package studentConsulting.model.payload.dto;

import java.time.LocalDateTime;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RatingDTO {
	private Integer departmentId;
    private String userName;  
    private String consultantName;
    
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

    private LocalDateTime submittedAt;
}
