package studentConsulting.model.payload.request.consultant;

import java.time.LocalDate;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ManageCreateConsultantScheduleRequest {
	private String title;
    private String content;
    private LocalDate consultationDate; 
    private String consultationTime; 
    private String location;
    private String link;
    private Boolean mode; 
}
