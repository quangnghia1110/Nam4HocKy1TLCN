//CÁI NÀY THỰC HIỆN ĐẦU TIÊN
//JwtTokenFilter và UserServiceImpl sẽ gọi đến 

package studentConsulting.security.JWT;

import io.jsonwebtoken.*;
import studentConsulting.model.entity.authentication.UserInformationEntity;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Date;

import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;

@Component
public class JwtProvider {
    private static final Logger logger = LoggerFactory.getLogger(JwtProvider.class);

    @Value("${jwt.secret}")
    private String jwtSecret;

    private static final int jwtExpirationMs = 10000; // 15 phút
    private static final long refreshTokenExpirationMs = 2592000000L; // 1 tháng (30 ngày)

    //Tạo token
    public String createToken(UserInformationEntity userModel) {
        // Kiểm tra đầu vào
        if (userModel == null || userModel.getAccount() == null) {
            throw new IllegalArgumentException("User model or account model is null (trong JwtProvider)");
        }

        // Tạo chuỗi JWT bằng thư viện jjwt
        String jwt = Jwts.builder()
                // Đặt subject của JWT là username của accountModel
                .setSubject(userModel.getAccount().getUsername())
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

        // In ra console để kiểm tra mã JWT đã tạo
        System.out.println("Generated JWT: " + jwt);

        // Trả về chuỗi JWT đã ký
        return jwt;
    }


    public String refreshToken(String oldToken) {
        try
        {
        	//Parse và lấy thông tin từ oldToken sử dụng Jwts.parser().
            Claims claims = Jwts.parser().setSigningKey(jwtSecret).parseClaimsJws(oldToken).getBody();
            String username = claims.getSubject();
            String authorities = (String) claims.get("authorities");
            Date issuedAt = claims.getIssuedAt();
            return Jwts.builder()
            		.setSubject(username)
            		.setIssuedAt(issuedAt)
            		.setExpiration(new Date(System.currentTimeMillis() + refreshTokenExpirationMs))
                    .signWith(SignatureAlgorithm.HS512, jwtSecret)
                    .claim("authorities", authorities)
                    .compact();
        }
        catch (Exception ex)
        {
            return "Có lỗi. Vui lòng thử lại";
        }
    }

    
    public boolean validateToken(String token) {
        try {
            // Đầu tiên parseClaimsJws nhận vào chuỗi token và thực hiện parse
            // Trước khi parse, setSigningkey sẽ kiểm tra chữ ký của jwt
            // Sau khi parse xong thì các claims sẽ có thể truy xuất đến getBody.getSubject,..
            Jwts.parser().setSigningKey(jwtSecret).parseClaimsJws(token);
            return true;
        } catch (SignatureException e) {
            logger.error("Invalid JWT signature! (trong JwtProvider)", e);
            throw new Exceptions.JWT401Exception("Chữ ký JWT không hợp lệ.");
        } catch (MalformedJwtException e) {
            logger.error("The token invalid format! (trong JwtProvider)", e);
            throw new Exceptions.JWT401Exception("Định dạng JWT không hợp lệ.");
        } catch (UnsupportedJwtException e) {
            logger.error("Unsupported jwt token (trong JwtProvider)", e);
            throw new Exceptions.JWT401Exception("JWT không được hỗ trợ.");
        } catch (ExpiredJwtException e) {
            logger.error("Expired jwt token (trong JwtProvider)", e);
            throw new Exceptions.JWT401Exception("JWT hết hạn. Vui lòng đăng nhập lại.");
        } catch (IllegalArgumentException e) {
            logger.error("JWT claims string is empty (trong JwtProvider)", e);
            throw new Exceptions.JWT401Exception("Chuỗi claims JWT rỗng.");
        }
    }


    //Trích xuất username từ jwt
    public String getUserNameFromToken(String token){
        String userName = Jwts.parser().setSigningKey(jwtSecret).parseClaimsJws(token).getBody().getSubject();
        return userName;
    }
    
    //Trích xuất ID của người dùng từ jwt 
    public Long getIdUserFromToken(String token)
    {
        String id = Jwts.parser().setSigningKey(jwtSecret).parseClaimsJws(token).getBody().getSubject();
        return Long.parseLong(id);
    }
}
