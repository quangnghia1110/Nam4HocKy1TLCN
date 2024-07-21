//Ngoại lệ để báo cáo khi một yêu cầu làm mới token bị từ chối.
//refreshToken.getExpiryDate().compareTo(Instant.now()) < 0
package studentConsulting.model.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;
//Không có quyền truy cập 403
@ResponseStatus(HttpStatus.FORBIDDEN)
public class RefreshTokenException extends RuntimeException {
	private static final long serialVersionUID = 1L;

	public RefreshTokenException(String message) {
        super(message);
    }

}
