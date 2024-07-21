//CÁI NÀY THỰC HIỆN THỨ 4
//JwtTokenFilter sẽ gọi đến

package studentConsulting.security.userPrinciple;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.authentication.AccountEntity;
import studentConsulting.model.entity.authentication.UserEntity;
import studentConsulting.repository.authentication.AccountRepository;
import studentConsulting.repository.authentication.UserRepository;

@Service
// interface UserDetailsService trong Spring Security
// được sử dụng để cung cấp thông tin chi tiết về người dùng từ một nguồn dữ liệu như cơ sở dữ liệu
public class UserDetailService implements UserDetailsService {

    @Autowired
    AccountRepository accountRepository;

    @Autowired
    UserRepository userRepository;

    // phương thức này được sử dụng để tìm kiếm thông tin người dùng trong nguồn dữ liệu
    // và tạo đối tượng UserDetails tương ứng
    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
        AccountEntity accountModel = accountRepository.findAccountByUsername(username);
        if(accountModel == null || accountModel.getId() <= 0)
        {
            throw new UsernameNotFoundException("Không tìm thấy tài khoản" + username);
        }
        UserEntity userModel = userRepository.findUserInfoModelByAccountModel(accountModel);
        return UserPrinciple.build(userModel);
    }
}
