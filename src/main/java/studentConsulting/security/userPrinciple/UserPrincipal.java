//AuthController sẽ gọi đến
package studentConsulting.security.userPrinciple;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
//Lớp này chứa một phương thức getUserId() để trả về userId của người dùng hiện tại từ SecurityContextHolder
//Lấy thông tin về người dùng đang xác thực trong phạm vi hiện tại của ứng dụng.
public class UserPrincipal {
	public Integer getUserId() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        // Kiểm tra xem người dùng đã được xác thực chưa
        if (authentication != null && authentication.getPrincipal() instanceof UserPrinciple) {
            UserPrinciple userPrinciple = (UserPrinciple) authentication.getPrincipal();
            return Integer.parseInt(userPrinciple.getUserId()); // Chuyển đổi từ String sang Integer
        }
        // Trả về null hoặc ném ngoại lệ nếu không thể lấy thông tin người dùng
        return null;
    }
}
