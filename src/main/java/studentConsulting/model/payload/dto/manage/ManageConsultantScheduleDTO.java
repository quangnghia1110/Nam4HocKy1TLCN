package studentConsulting.model.payload.dto.manage;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ManageConsultantScheduleDTO {
    private Integer id;
    private String title;
    private String content;
    private LocalDate consultationDate;
    private String consultationTime;
    private String location;
    private String link;
    private Boolean mode;
    private Boolean statusPublic;
    private Boolean statusConfirmed;
    private Integer created_by;
    private Boolean type;
}
