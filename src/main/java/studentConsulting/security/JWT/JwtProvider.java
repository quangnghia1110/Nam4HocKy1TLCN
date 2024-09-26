//CÁI NÀY THỰC HIỆN ĐẦU TIÊN
//JwtTokenFilter và UserServiceImpl sẽ gọi đến 

package studentConsulting.security.JWT;

import io.jsonwebtoken.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.exception.Exceptions;

import java.util.Date;

@Component
public class JwtProvider {
    private static final Logger logger = LoggerFactory.getLogger(JwtProvider.class);
    private static final int jwtExpirationMs = 900000; // 15 phút
    private static final long refreshTokenExpirationMs = 2592000000L; // 1 tháng (30 ngày)
    @Value("${jwt.secret}")
    private String jwtSecret;

    //Tạo token
    public String createToken(UserInformationEntity userModel) {
        if (userModel == null || userModel.getAccount() == null) {
            throw new IllegalArgumentException("User model or account model is null (trong JwtProvider)");
        }

        // Tạo chuỗi JWT bằng thư viện jjwt
        String jwt = Jwts.builder()
                // Đặt subject của JWT là email của accountModel
                .setSubject(userModel.getAccount().getEmail())
                // Đặt thời điểm phát hành JWT là thời điểm hiện tại
                .setIssuedAt(new Date(System.currentTimeMillis()))
                // Đặt thời điểm hết hạn của JWT là sau 15 phút từ thời điểm hiện tại
                .setExpiration(new Date(System.currentTimeMillis() + jwtExpirationMs))
                // Ký JWT bằng thuật toán HS512 và secret key
                .signWith(SignatureAlgorithm.HS512, jwtSecret)
                // Thêm claim vào JWT để lưu trữ quyền của người dùng (authorities)
                .claim("authorities", userModel.getAccount().getRole().getName().replace("ROLE_", ""))
                // Kết thúc quá trình tạo JWT và trả về chuỗi JWT đã ký
                .compact();

        return jwt;
    }


    public String refreshToken(String oldToken) {
        try {
            Claims claims = Jwts.parser().setSigningKey(jwtSecret).parseClaimsJws(oldToken).getBody();
            String username = claims.getSubject();
            String authorities = (String) claims.get("authorities");

            return Jwts.builder()
                    .setSubject(username)
                    .setIssuedAt(new Date())
                    .setExpiration(new Date(System.currentTimeMillis() + refreshTokenExpirationMs))
                    .signWith(SignatureAlgorithm.HS512, jwtSecret)
                    .claim("authorities", authorities)
                    .compact();
        } catch (Exception ex) {
            throw new Exceptions.JWT401Exception("Lỗi khi làm mới JWT. Vui lòng đăng nhập lại.", "ERROR_REFRESH");
        }
    }

    public boolean validateToken(String token) {
        try {
            Jwts.parser().setSigningKey(jwtSecret).parseClaimsJws(token);
            return true;
        } catch (SignatureException e) {
            throw new Exceptions.JWT401Exception("Chữ ký JWT không hợp lệ", "INVALID_SIGNATURE");
        } catch (UnsupportedJwtException e) {
            throw new Exceptions.JWT401Exception("JWT không được hỗ trợ", "UNSUPPORTED_JWT");
        } catch (ExpiredJwtException e) {
            throw new Exceptions.JWT401Exception("JWT hết hạn. Vui lòng đăng nhập lại", "EXPIRE_TOKEN");
        } catch (IllegalArgumentException e) {
            throw new Exceptions.JWT401Exception("Chuỗi claims JWT rỗng", "EMPTY_CLAIMS");
        }
    }


    public String getEmailFromToken(String token) {
        String email = Jwts.parser().setSigningKey(jwtSecret).parseClaimsJws(token).getBody().getSubject();
        return email;
    }

    public Long getIdUserFromToken(String token) {
        String id = Jwts.parser().setSigningKey(jwtSecret).parseClaimsJws(token).getBody().getSubject();
        return Long.parseLong(id);
    }
}
