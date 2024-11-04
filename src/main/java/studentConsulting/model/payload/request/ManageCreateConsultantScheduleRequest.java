package studentConsulting.model.payload.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

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
