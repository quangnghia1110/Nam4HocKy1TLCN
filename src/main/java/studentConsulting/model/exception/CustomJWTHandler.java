package studentConsulting.model.exception;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.web.access.AccessDeniedHandler;
import org.springframework.stereotype.Component;

@Component
public class CustomJWTHandler implements AccessDeniedHandler {

    @Override
    public void handle(HttpServletRequest request, HttpServletResponse response, AccessDeniedException accessDeniedException) throws IOException {
        // Đặt trạng thái HTTP là 401 (Unauthorized)
        response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
        
        // Thiết lập kiểu nội dung là JSON và mã hóa UTF-8
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        
        // Tạo chuỗi JSON trả về thông báo lỗi
        String jsonResponse = "{\"error\": \"401\", \"message\": \"Người dùng không có quyền truy cập\"}";
        
        // Ghi chuỗi JSON vào response
        response.getWriter().write(jsonResponse);
    }
}
