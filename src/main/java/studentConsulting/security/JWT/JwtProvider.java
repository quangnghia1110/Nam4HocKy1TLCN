//CÁI NÀY THỰC HIỆN ĐẦU TIÊN
//JwtTokenFilter và UserServiceImpl sẽ gọi đến 

package studentConsulting.security.JWT;

import com.google.api.client.http.HttpStatusCodes;
import io.jsonwebtoken.*;
import studentConsulting.model.entity.authentication.UserInformationEntity;

import org.hibernate.service.spi.ServiceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Date;

@Component
public class JwtProvider {
    private static final Logger logger = LoggerFactory.getLogger(JwtProvider.class);

    @Value("${jwt.secret}")
    private String jwtSecret;

    private static final int jwtExpirationMs = 3600000; //1 tiếng

    //Tạo token
    public String createToken(UserInformationEntity userModel) {
        // Kiểm tra đầu vào
        if (userModel == null || userModel.getAccountModel() == null) {
            throw new IllegalArgumentException("User model or account model is null (trong JwtProvider)");
        }

        // Tạo chuỗi JWT bằng thư viện jjwt
        String jwt = Jwts.builder()
                // Đặt subject của JWT là username của accountModel
                .setSubject(userModel.getAccountModel().getUsername())
                // Đặt thời điểm phát hành JWT là thời điểm hiện tại
                .setIssuedAt(new Date(System.currentTimeMillis()))
                // Đặt thời điểm hết hạn của JWT là sau 60 phút từ thời điểm hiện tại
                .setExpiration(new Date(System.currentTimeMillis() + jwtExpirationMs))
                // Ký JWT bằng thuật toán HS512 và secret key
                .signWith(SignatureAlgorithm.HS512, jwtSecret)
                // Thêm claim vào JWT để lưu trữ quyền của người dùng (authorities)
                .claim("authorities", userModel.getAccountModel().getRoleModel().getName())
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
            		.setExpiration(new Date(System.currentTimeMillis() + jwtExpirationMs))
                    .signWith(SignatureAlgorithm.HS512, jwtSecret)
                    .claim("authorities", authorities)
                    .compact();
        }
        catch (Exception ex)
        {
            return "Có lỗi. Vui lòng thử lại";
        }
    }

    
    public boolean validateToken(String token)
    {
        try {
        	//Đầu tiên parseClaimsJws nhận vào chuỗi token và thực hiện parse
        	//Trước khi parse, setSigningkey sẽ kiểm tra chữ ký của jwt
        	//Sau khi parse xong thÌ các claims sẽ có thể truy xuất đến getBody.getSubject,..
            Jwts.parser().setSigningKey(jwtSecret).parseClaimsJws(token);
            return true;
        }
        catch (SignatureException e)
        {
            logger.error("Invalid JWT signature! (trong JwtProvider)", e);
        }
        catch (MalformedJwtException e)
        {
            logger.error("The token invalid format! (trong JwtProvider)", e);
        }
        catch (UnsupportedJwtException e)
        {
            logger.error("Unsupported jwt token (trong JwtProvider)", e);
        }
        catch (ExpiredJwtException e)
        {
            logger.error("Expired jwt token (trong JwtProvider)", e);
        }
        catch (IllegalArgumentException e)
        {
            logger.error("JWT claims string is empty (trong JwtProvider)", e);
        }
        return false;
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
