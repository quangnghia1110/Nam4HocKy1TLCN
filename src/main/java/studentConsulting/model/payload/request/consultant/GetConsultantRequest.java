package studentConsulting.model.payload.request.consultant;

import lombok.Builder;
import lombok.Data;

@Builder
@Data
public class GetConsultantRequest {
	private String firstName;
	private String lastName;
	private String phone;
	private String email;
	private String avatarUrl;
	private Integer departmentId;
}
