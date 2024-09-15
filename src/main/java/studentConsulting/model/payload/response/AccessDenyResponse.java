package studentConsulting.model.payload.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccessDenyResponse {

	@Builder.Default
	private String status = "error";
	private String message;

}
