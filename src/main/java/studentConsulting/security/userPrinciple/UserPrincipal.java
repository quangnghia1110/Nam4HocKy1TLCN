//AuthController sẽ gọi đến
package studentConsulting.security.userPrinciple;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
//Lớp này chứa một phương thức getUserId() để trả về userId của người dùng hiện tại từ SecurityContextHolder
//Lấy thông tin về người dùng đang xác thực trong phạm vi hiện tại của ứng dụng.
public class UserPrincipal {
    public Long getUserId()
    {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        UserPrinciple userPrinciple = (UserPrinciple) authentication.getPrincipal();
        return Long.parseLong(userPrinciple.getUserId());
    }
}
