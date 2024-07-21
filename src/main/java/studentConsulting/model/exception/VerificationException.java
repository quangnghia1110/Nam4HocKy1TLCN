//Ngoại lệ để báo cáo khi một yêu cầu xác minh không thành công.
//Optional<Verification> verifyOpt = verificationRepository.findByUserId(userId);
package studentConsulting.model.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.UNAUTHORIZED)
public class VerificationException extends RuntimeException {
	private static final long serialVersionUID = 1L;

	public VerificationException(String message) {
        super(message);
    }

}
