package studentConsulting.security.JWT;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.stereotype.Component;
import studentConsulting.model.exception.CustomAuthenticationException;
import studentConsulting.model.exception.JWT401Exception;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

@Component
public class JwtEntryPoint implements AuthenticationEntryPoint {

    @Override
    public void commence(HttpServletRequest request, HttpServletResponse response, AuthenticationException authException) throws IOException {
        response.setContentType("application/json;charset=UTF-8");
        response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);

        // Kiểm tra nếu ngoại lệ là loại của CustomAuthenticationException
        if (authException instanceof CustomAuthenticationException) {
            JWT401Exception jwtException = ((CustomAuthenticationException) authException).getJwt401Exception();

            // Tạo một Map chứa thông tin lỗi
            Map<String, Object> body = new HashMap<>();
            body.put("status", jwtException.getStatus());
            body.put("message", jwtException.getMessage());
            body.put("type", jwtException.getType());

            // Sử dụng ObjectMapper để chuyển Map thành JSON
            final ObjectMapper mapper = new ObjectMapper();
            mapper.writeValue(response.getOutputStream(), body);
        } else {
            // Trong trường hợp ngoại lệ không phải là CustomAuthenticationException
            Map<String, Object> body = new HashMap<>();
            body.put("status", HttpServletResponse.SC_UNAUTHORIZED);
            body.put("message", authException.getMessage());
            body.put("type", "UNAUTHORIZED");

            final ObjectMapper mapper = new ObjectMapper();
            mapper.writeValue(response.getOutputStream(), body);
        }
    }
}

