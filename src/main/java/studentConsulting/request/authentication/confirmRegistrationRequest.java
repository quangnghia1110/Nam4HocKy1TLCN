package studentConsulting.request.authentication;
import lombok.Data;

@Data
public class confirmRegistrationRequest {
    private String emailRequest;
	private String token;
}
