package studentConsulting.request.authentication;

import lombok.Data;

@Data
public class verifyCodeCheckRequest {
    private String emailRequest;

    private String code;
}
