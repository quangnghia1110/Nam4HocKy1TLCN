package studentConsulting.model.exception;

import org.springframework.security.core.AuthenticationException;

public class CustomAuthenticationException extends AuthenticationException {

    private final JWT401Exception jwt401Exception;

    public CustomAuthenticationException(JWT401Exception jwt401Exception) {
        super(jwt401Exception.getMessage());
        this.jwt401Exception = jwt401Exception;
    }

    public JWT401Exception getJwt401Exception() {
        return jwt401Exception;
    }
}