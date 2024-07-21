//Ngoại lệ để báo cáo khi một token đã hết hạn
//boolean tokenIsExpired = verification.getToken().equals(registration.getToken())
//                    && !verification.getExpiredAt().isAfter(Instant.now())
//                    && verification.getType().equals(VerificationType.REGISTRATION);

package studentConsulting.model.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.UNAUTHORIZED)
public class ExpiredTokenException extends RuntimeException {
	private static final long serialVersionUID = 1L;

	public ExpiredTokenException(String message) {
        super(message);
    }

}
