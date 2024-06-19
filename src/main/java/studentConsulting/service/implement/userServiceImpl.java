package studentConsulting.service.implement;

import com.google.api.client.util.DateTime;

import studentConsulting.entity.authentication.accountEntity;
import studentConsulting.entity.authentication.roleAuthEntity;
import studentConsulting.entity.authentication.roleEntity;
import studentConsulting.entity.authentication.userEntity;
import studentConsulting.repository.authentication.accountRepository;
import studentConsulting.repository.authentication.roleAuthRepository;
import studentConsulting.repository.authentication.roleRepository;
import studentConsulting.repository.authentication.userRepository;
import studentConsulting.request.authentication.changePasswordRequest;
import studentConsulting.request.authentication.confirmRegistrationRequest;
import studentConsulting.request.authentication.forgotPasswordRequest;
import studentConsulting.request.authentication.loginRequest;
import studentConsulting.request.authentication.registerRequest;
import studentConsulting.request.authentication.resetPasswordRequest;
import studentConsulting.request.authentication.updateInformationRequest;
import studentConsulting.request.authentication.verifyCodeCheckRequest;
import studentConsulting.response.apiResponse;
import studentConsulting.response.loginResponse;
import studentConsulting.response.registerResponse;
import studentConsulting.security.JWT.JwtProvider;
import studentConsulting.service.interfaces.userServiceInterface;
import studentConsulting.util.CurrentDateTime;
import studentConsulting.util.RandomUtils;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import javax.mail.internet.MimeMessage;

import java.time.Duration;
import java.util.Optional;
import java.util.UUID;

@Service
public class userServiceImpl implements userServiceInterface {

    private final long expireInRefresh = Duration.ofHours(10).toMillis();
    @Autowired
    roleRepository roleRepository;

    @Autowired
    accountRepository accountRepository;

    @Autowired
    userRepository userRepository;

    @Autowired
    PasswordEncoder passwordEncoder;

    @Autowired
    private JwtProvider jwtProvider;

    @Autowired
    private roleAuthRepository tokenRepository;

    @Autowired
    private JavaMailSender javaMailSender;

    @Autowired
    private emailServiceImpl emailService;

    // build token
    //Tạo ra và lưu trữ một token mới, sau đó trả về thông tin phản hồi đăng nhập bao gồm token truy cập, 
    //thời gian hết hạn, và mã định danh token để người dùng có thể sử dụng trong các yêu cầu tiếp theo.
    private loginResponse buildToken(userEntity userModel) {
        String jti = UUID.randomUUID().toString();
        long expiredTime = System.currentTimeMillis() + expireInRefresh;
        
        // Lưu token vào repository
        tokenRepository.save(roleAuthEntity.builder()
                .userModel(userModel)
                .tokenId(jti)
                .expiredTime(expiredTime)
                .build());
        
        // Tạo access token
        String accessToken = jwtProvider.createToken(userModel);
        
        // In giá trị ra console
        System.out.println("User Model: " + userModel);
        System.out.println("Access Token: " + accessToken);
        System.out.println("JTI (Refresh Token): " + jti);
        System.out.println("Expired Time: " + expiredTime);

        // Trả về đối tượng loginResponse
        return loginResponse.builder()
                .userModel(userModel)
                .accessToken(accessToken) // Access Token để truy cập tài nguyên
                .expiresIn(expiredTime) // Thời gian hết hạn của Access Token
                .refreshToken(jti) // Refresh Token để làm mới Access Token
                .status(true)
                .build();

    }

    @Override
    public loginResponse refreshToken(String refreshToken)
    {
        roleAuthEntity tokenModel = tokenRepository.findByTokenId(refreshToken);
        if(tokenModel == null || tokenModel.getId() <= 0)
        {
            return loginResponse.builder().message("Refresh token is not exists").status(false).build();
        }
        else {
            if(System.currentTimeMillis() > tokenModel.getExpiredTime())
            {
                return loginResponse.builder().message("Jwt refresh token expired at " + new DateTime(tokenModel.getExpiredTime())).status(false).build();
            }
            Optional<userEntity> userModel = userRepository.findById(tokenModel.getUserModel().getId());
            return buildToken(userModel.get());
        }
    }
    
    @Override
    public loginResponse login(loginRequest loginRequest)
    {
        accountEntity accountModel = accountRepository.findAccountByUsername(loginRequest.getUsername());
        if(accountModel == null || accountModel.getId() == null)
        {
            return loginResponse.builder().message("Tài khoản không hợp lệ!").status(false).build();
        }
        else if(accountModel.isActivity() == false)
        {
            return loginResponse.builder().message("Tài khoản đã bị khóa!").status(false).build();
        }
        else if(passwordEncoder.matches(loginRequest.getPassword(), accountModel.getPassword()))
        {
            return buildToken(userRepository.findUserInfoModelByAccountModel(accountModel));
        }
        else {
            return loginResponse.builder().message("Tài khoản hoặc mật khẩu không hợp lệ. Vui lòng thử lại").status(false).build();
        }
    }
    String confirmationUrl;
    @Override
    public registerResponse register(registerRequest registerRequest) {
        // Check if the account already exists by username
        accountEntity accountModel = accountRepository.findAccountByUsername(registerRequest.getUserName());
        if (accountModel != null && accountModel.getId() >= 0) {
            return registerResponse.builder()
                    .status(false)
                    .message("Tài khoản đã tồn tại. Vui lòng nhập lại!")
                    .build();
        }

        // Check if the email already exists
        if (accountRepository.existsByEmail(registerRequest.getEmail())) {
            return registerResponse.builder()
                    .status(false)
                    .message("Email đã tồn tại. Vui lòng nhập lại!")
                    .build();
        }

        // Check if the role exists
        roleEntity roleModel = roleRepository.findByName(registerRequest.getRoleName());
        if (roleModel == null) {
            return registerResponse.builder()
                    .status(false)
                    .message("Role không tồn tại. Vui lòng kiểm tra lại!")
                    .build();
        }

        // Encode password
        String hashedPassword = passwordEncoder.encode(registerRequest.getPassWord());

        // Create and save AccountEntity
        String verifyToken = UUID.randomUUID().toString(); // Generate random verification token
        accountModel = accountEntity.builder()
                .username(registerRequest.getUserName())
                .roleModel(roleModel)
                .email(registerRequest.getEmail())
                .password(hashedPassword)
                .isActivity(false) 
                .build();

        // Create and save UserEntity
        userEntity userModel = userEntity.builder()
                .firstname(registerRequest.getFirstname())
                .lastname(registerRequest.getLastname())
                .phone(registerRequest.getPhone())
                .occupation(registerRequest.getOccupation())
                .accountModel(accountModel)
                .build();

        // Send registration confirmation email
        String verifyTokens = RandomUtils.getRandomVerifyCode();

        confirmationUrl = verifyTokens;
        String emailContent = "<html><body><p>Xin chào " + userModel.getLastname() + ",</p>"
                + "<p>Cảm ơn bạn đã đăng ký tài khoản. Vui lòng mã sau để xác nhận đăng ký:</p>"
                + "<p>" + confirmationUrl +"</p>"
                + "<p>Trân trọng,</p><p>Ban quản trị</p></body></html>";

        try {
            MimeMessage mailMessage = javaMailSender.createMimeMessage();
            MimeMessageHelper mailHelper = new MimeMessageHelper(mailMessage, true, "UTF-8");
            mailHelper.setFrom("ngoquangnghia111003@gmail.com");
            mailHelper.setTo(registerRequest.getEmail());
            mailHelper.setSubject("Xác nhận đăng ký tài khoản");
            mailHelper.setText(emailContent, true);
            javaMailSender.send(mailMessage);
            accountRepository.save(accountModel);
            userRepository.save(userModel);
        } catch (Exception e) {
            return registerResponse.builder()
                    .status(false)
                    .message("Lỗi gửi email xác nhận đăng ký!")
                    .build();
        }

        // Return success response with userModel
        return registerResponse.builder()
                .status(true)
                .message("Đăng ký thành công! Vui lòng kiểm tra email để xác nhận đăng ký.")
                .userModel(userModel)
                .build();
    }


    //Constructor apiResponse(int status)
    @Override
    public apiResponse<Object> changePassword(String username, changePasswordRequest changePasswordRequest)
    {
        accountEntity account = accountRepository.findAccountByUsername(username);
        if(passwordEncoder.matches(changePasswordRequest.getPassword(), account.getPassword()) == false)
        {
            return apiResponse.builder().status(101).message("Nhập sai mật khẩu cũ").build();
        }
        String hashedPassword = passwordEncoder.encode(changePasswordRequest.getNewPassword());
        account.setPassword(hashedPassword);
        accountRepository.save(account);
        return apiResponse.builder().status(200).message("Thay đổi mật khẩu thành công").build();
    }

    @Override
    public apiResponse<Object> forgotPassword(forgotPasswordRequest forgotPasswordRequest)
    {
        accountEntity account = accountRepository.findAccountByEmail(forgotPasswordRequest.getEmailRequest());
        if(account == null || account.getId() <= 0)
        {
            return apiResponse.builder().status(101).message("Email không tồn tại trong hệ thống!").build();
        }
        else
        {
            userEntity userInformation = userRepository.findUserInfoModelByAccountModel(account);
            String verifyCode = RandomUtils.getRandomVerifyCode();
            try {
                MimeMessage mailMessage = javaMailSender.createMimeMessage();
                MimeMessageHelper mailHelper = new MimeMessageHelper(mailMessage, true, "UTF-8");
                mailHelper.setFrom("ngoquangnghia111003@gmail.com");
                mailHelper.setTo(forgotPasswordRequest.getEmailRequest());
                mailHelper.setSubject("Mã xác nhận lấy lại mật khẩu");
                mailHelper.setText("<html><body><p>Xin chào! " + userInformation.getLastname()
                        + "</p><p>Bạn đã yêu cầu cập nhật lại mật khẩu</p>"
                        + "<p>Đây là mã xác nhận lấy lại mật khẩu của bạn: </p>" + "<h2><b>" + verifyCode + "</b></h2>"
                        + "</body></html>" , true);
                emailService.sendEmail(mailMessage);
                account.setVerifyCode(verifyCode);
                accountRepository.save(account);
            } catch (Exception e)
                {
                    return apiResponse.builder().status(101).message("Lỗi gửi verify code!").build();
                }
        }
        return apiResponse.builder().status(200).message("Mã xác nhận đã được gửi qua email!").build();
    }

    @Override
    public apiResponse<Object> checkVerifyCode(verifyCodeCheckRequest verifyCode)
    {
        accountEntity account = accountRepository.findAccountByEmail(verifyCode.getEmailRequest());
        if(account == null || account.getId() <= 0)
        {
            return apiResponse.builder().status(400).message("Không tìm thấy tài khoản").build();
        }
        if(!account.getVerifyCode().equals(verifyCode.getCode()))
        {
            return apiResponse.builder().status(101).message("Sai mã xác thực!").build();
        }
        return apiResponse.builder().status(200).message("Xác nhận thành công!").build();
    }

    @Override
    public apiResponse<Object> resetPassword(resetPasswordRequest resetPasswordRequest)
    {
        accountEntity account = accountRepository.findAccountByEmail(resetPasswordRequest.getEmail());
        if(account == null || account.getId() <= 0)
        {
            return apiResponse.builder().status(400).message("Không tìm thấy tài khoản").build();
        }
        String hashedPassword = passwordEncoder.encode(resetPasswordRequest.getNewPassword());
        account.setPassword(hashedPassword);
        accountRepository.save(account);
        return apiResponse.builder().status(200).message("Lấy lại mật khẩu thành công").build();
    }

    @Override
    public Iterable<userEntity> getAllUser()
    {
        return userRepository.findAllByRoleName("USER");
    }

    //Constructor apiResponse(int status) và apiResponse(T data)
    @Override
    public apiResponse<Object> getProfile(Long idUser)
    {
        Optional<userEntity> userInformation = userRepository.findById(idUser);
        if(!userInformation.isPresent())
        {
            return apiResponse.builder().status(404).message("Không tìm thấy tài khoản").build();
        }
        return apiResponse.builder().status(200).message("Thông tin người dùng").data(userInformation.get()).build();
    }

    @Override
    public apiResponse<Object> updateProfile(Long idUser, updateInformationRequest userUpdateRequest)
    {
        Optional<userEntity> userInformation = userRepository.findById(idUser);
        if(!userInformation.isPresent())
        {
            return apiResponse.builder().status(404).message("Không tìm thấy tài khoản").build();
        }
        userInformation.get().setFirstname(userUpdateRequest.getFirstname());
        userInformation.get().setLastname(userUpdateRequest.getLastname());
        userRepository.save(userInformation.get());
        return apiResponse.builder().status(200).message("Thay đổi thông tin thành công").build();
    }

    @Override
    public apiResponse<Object> deleteUser(Long idUser)
    {
        Optional<userEntity> userInformation = userRepository.findById(idUser);
        if(userInformation.get() == null)
        {
            return apiResponse.builder().status(404).message("Không tim thấy người dùng").build();
        }
        userInformation.get().getAccountModel().setActivity(false);
        userRepository.save(userInformation.get());
        return apiResponse.builder().status(200).message("Khóa người dùng thành công").build();
    }
    
    @Override
    public apiResponse<Object> unlockUser(Long idUser)
    {
        Optional<userEntity> userInformation = userRepository.findById(idUser);
        if(userInformation.get() == null)
        {
            return apiResponse.builder().status(404).message("Không tim thấy người dùng").build();
        }
        userInformation.get().getAccountModel().setActivity(true);
        userRepository.save(userInformation.get());
        return apiResponse.builder().status(200).message("Mở khóa người dùng thành công").build();
    }
    
    @Override
    public apiResponse<Object> confirmRegistration(confirmRegistrationRequest confirmRegistrationRequest) {
        // Tìm accountEntity dựa trên email từ request
        accountEntity account = accountRepository.findAccountByEmail(confirmRegistrationRequest.getEmailRequest());
        
        // Kiểm tra xem có tìm thấy account hay không
        if (account == null || account.getId() <= 0) {
            // Nếu không tìm thấy tài khoản
            return apiResponse.builder().status(400).message("Không tìm thấy tài khoản").build();
        } 
       // Đặt lại trạng thái activity là true và xoá verifyCode
        account.setActivity(true);
        account.setVerifyRegister(confirmationUrl);
        account = accountRepository.save(account);

        // Trả về thông báo xác nhận thành công
        return apiResponse.builder().status(200).message("Xác nhận thành công!!!").build();
    }




}
