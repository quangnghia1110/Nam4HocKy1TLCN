package studentConsulting.model.payload.request.authentication;

import lombok.Data;

@Data
public class VerifyCodeCheckRequest {
    private String emailRequest;

    private String code;
}
