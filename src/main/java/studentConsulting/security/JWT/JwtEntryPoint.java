//CÁI NÀY THỰC HIỆN THỨ 3
//Security sẽ gọi đến
package studentConsulting.security.JWT;

import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.stereotype.Component;
import com.fasterxml.jackson.databind.ObjectMapper;
import studentConsulting.model.exception.Exceptions.ErrorException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;

@Component
public class JwtEntryPoint implements AuthenticationEntryPoint {

    @Override
    public void commence(HttpServletRequest request, HttpServletResponse response, AuthenticationException authException) throws IOException {
        // Tạo thông báo lỗi tùy chỉnh
        Map<String, Object> body = new HashMap<>();
        body.put("timestamp", LocalDateTime.now().format(DateTimeFormatter.ISO_DATE_TIME));
        body.put("status", HttpServletResponse.SC_UNAUTHORIZED);  // Mã trạng thái 401
        body.put("error", "Unauthorized");
        body.put("message", "Error");
        body.put("path", request.getRequestURI());

        // Cấu hình phản hồi
        response.setContentType("application/json");
        response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);  // Mã trạng thái 401
        final ObjectMapper mapper = new ObjectMapper();
        mapper.writeValue(response.getOutputStream(), body);
    }
}

