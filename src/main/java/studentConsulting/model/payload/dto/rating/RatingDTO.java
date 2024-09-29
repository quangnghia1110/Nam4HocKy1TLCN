package studentConsulting.model.payload.dto.rating;

import java.time.LocalDate;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.payload.dto.department_field.DepartmentDTO;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RatingDTO {
	private DepartmentDTO department;
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

    private LocalDate submittedAt;
}
