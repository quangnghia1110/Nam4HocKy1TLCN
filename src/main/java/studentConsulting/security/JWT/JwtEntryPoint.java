//CÁI NÀY THỰC HIỆN THỨ 3
//Security sẽ gọi đến
package studentConsulting.security.JWT;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.stereotype.Component;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

@Component
public class JwtEntryPoint implements AuthenticationEntryPoint {

    private static final Logger logger= LoggerFactory.getLogger(JwtTokenFilter.class);
    @Override
    public void commence(HttpServletRequest request, HttpServletResponse response, AuthenticationException authException) throws IOException, ServletException {
        logger.error("Unauthorized lỗi trong JwtEntryPoint", authException.getMessage());
        response.sendError(HttpServletResponse.SC_ACCEPTED, "Error");
    }
}
