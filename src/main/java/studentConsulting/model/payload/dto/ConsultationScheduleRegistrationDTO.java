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
public class ConsultationScheduleRegistrationDTO {
    private Integer id;
    private Integer userId;
    private Integer consultationScheduleId;
    private LocalDate registeredAt;
    private Boolean status;
}

