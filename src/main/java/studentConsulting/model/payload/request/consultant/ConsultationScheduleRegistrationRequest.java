package studentConsulting.model.payload.request.consultant;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConsultationScheduleRegistrationRequest {
    private Integer consultationScheduleId;
}
