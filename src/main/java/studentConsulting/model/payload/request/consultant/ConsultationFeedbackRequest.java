package studentConsulting.model.payload.request.consultant;

import java.time.LocalDate;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ConsultationFeedbackRequest {
    private Integer consultationId;
    private Boolean statusConfirmed;  
    private String link;  
    private String location;
    private LocalDate consulationDate;
    private String consultationTime;  
    private Boolean mode; 
}
