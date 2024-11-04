package studentConsulting.model.payload.request;

import lombok.Data;

@Data
public class VerifyCodeCheckRequest {
    private String emailRequest;

    private String code;
}
