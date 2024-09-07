package studentConsulting.model.payload.request.consultant;

import lombok.Builder;
import lombok.Data;

@Builder
@Data
public class CreateScheduleConsultationRequest {
	private Integer consultantId;
    private Integer departmentId;
    private String title;
    private String content;
    private Boolean statusPublic;
}
