package studentConsulting.model.payload.request.consultant;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConsultationScheduleRegistrationRequest {
    private Integer userId;
    private Integer consultationScheduleId;
    private LocalDateTime registeredAt;
    private Boolean status;
}

