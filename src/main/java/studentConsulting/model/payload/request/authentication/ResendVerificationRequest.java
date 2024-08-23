package studentConsulting.model.payload.request.authentication;

import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ResendVerificationRequest {
    private String emailRequest;
}

