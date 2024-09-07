package studentConsulting.model.payload.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConsultationScheduleDTO {
    private Integer departmentId;
    private String userName;  
    private String consultantName;  
    private String title;
    private String content;
    private String consultationDate; 
    private String consultationTime; 
    private String location;
    private String link;
    private Boolean mode; 
    private Boolean statusPublic;
    private Boolean statusConfirmed;
}
