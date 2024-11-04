package studentConsulting.model.payload.request;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;

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
