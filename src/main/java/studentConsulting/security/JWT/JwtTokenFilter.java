//CÁI NÀY THỰC HIỆN THỨ 6
//Security sẽ gọi đến
package studentConsulting.security.JWT;

import lombok.extern.slf4j.Slf4j;
import studentConsulting.security.userPrinciple.UserDetailService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.web.filter.OncePerRequestFilter;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class JwtTokenFilter extends OncePerRequestFilter {

    private static final Logger logger= LoggerFactory.getLogger(JwtTokenFilter.class);
    @Autowired
    private UserDetailService userDetailService;

    @Autowired
    private JwtProvider jwtProvider;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        try {
            String token = getJwt(request);
            if(token != null && jwtProvider.validateToken(token))
            {
                String username = jwtProvider.getUserNameFromToken(token);
                UserDetails userDetails = userDetailService.loadUserByUsername(username);
                //Tạo một đối tượng UsernamePasswordAuthenticationToken với thông tin người dùng.
                UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken(
                        userDetails,null,userDetails.getAuthorities()
                );
                //Đặt chi tiết yêu cầu vào authenticationToken.
                authenticationToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                //Đặt thông tin xác thực vào SecurityContextHolder.
                SecurityContextHolder.getContext().setAuthentication(authenticationToken);
            }
        }
        catch (Exception e)
        {
            logger.error("Can't set user authentication", e);
        }
        //Chuyển tiếp request và response đến filter tiếp theo trong chuỗi filter.
        filterChain.doFilter(request, response);
    }

    //Lấy token từ 1 request
    private String getJwt(HttpServletRequest request)
    {
        String authHeader = request.getHeader("Authorization");
        if(authHeader != null && authHeader.startsWith("Bearer"))
        {
        	//Loại bỏ tiền tố "Bearer" và trả về token.
            return authHeader.replace("Bearer", "");
        }
        return null;
    }
}
