package studentConsulting.security.authentication;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.AccountEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.repository.admin.AccountRepository;
import studentConsulting.repository.admin.UserRepository;

import javax.transaction.Transactional;

@Service
public class UserDetailService implements UserDetailsService {

    @Autowired
    private AccountRepository accountRepository;

    @Autowired
    private UserRepository userRepository;

    @Override
    @Transactional
    public UserDetails loadUserByUsername(String email) throws UsernameNotFoundException {
        AccountEntity accountModel = accountRepository.findAccountByEmail(email);
        if (accountModel == null) {
            throw new UsernameNotFoundException("Không tìm thấy tài khoản với email: " + email);
        }

        UserInformationEntity userModel = userRepository.findUserInfoModelByAccountModel(accountModel);
        return UserPrinciple.build(userModel);
    }
}
//Xác tuwuc nguoi dung