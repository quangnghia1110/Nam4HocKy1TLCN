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

import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Jwts;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Date;

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
            if (token != null) {
                try {
                    if (jwtProvider.validateToken(token)) {
                        String username = jwtProvider.getUserNameFromToken(token);
                        UserDetails userDetails = userDetailService.loadUserByUsername(username);

                        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken(
                                userDetails, null, userDetails.getAuthorities());
                        authenticationToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));

                        SecurityContextHolder.getContext().setAuthentication(authenticationToken);
                    }
                } catch (ExpiredJwtException e) {
                    logger.info("JWT hết hạn. Thực hiện refresh token.");
                    String refreshToken = jwtProvider.refreshToken(token); // Làm mới token
                    response.setHeader("Authorization", "Bearer " + refreshToken); // Đặt token mới vào header phản hồi
                }
            }
        } catch (Exception e) {
            logger.error("Không thể đặt xác thực vào SecurityContext", e);
        }

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
