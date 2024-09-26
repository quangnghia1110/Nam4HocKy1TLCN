package studentConsulting.model.payload.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConsultationScheduleDTO {
    private Integer id;
    private DepartmentDTO department;
    private String userName;
    private String consultantName;
    private String title;
    private String content;
    private LocalDate consultationDate;
    private String consultationTime;
    private String location;
    private String link;
    private Boolean mode;
    private Boolean statusPublic;
    private Boolean statusConfirmed;
    private Integer createdBy;
}
