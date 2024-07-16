package studentConsulting.request.authentication;

import lombok.Data;

@Data
public class VerifyCodeCheckRequest {
    private String emailRequest;

    private String code;
}
