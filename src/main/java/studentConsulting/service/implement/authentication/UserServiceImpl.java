package studentConsulting.service.implement.authentication;

import java.sql.Timestamp;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;
import java.util.regex.Pattern;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;
import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import com.google.api.client.util.DateTime;

import studentConsulting.constant.FieldName;
import studentConsulting.constant.ResourceName;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.authentication.AccountEntity;
import studentConsulting.model.entity.authentication.RoleAuthEntity;
import studentConsulting.model.entity.authentication.RoleEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.Exceptions.ResourceNotFoundException;
import studentConsulting.model.payload.dto.AccountDTO;
import studentConsulting.model.payload.dto.AddressDTO;
import studentConsulting.model.payload.dto.UserInformationDTO;
import studentConsulting.model.payload.request.authentication.ChangeEmailRequest;
import studentConsulting.model.payload.request.authentication.ChangePasswordRequest;
import studentConsulting.model.payload.request.authentication.ConfirmRegistrationRequest;
import studentConsulting.model.payload.request.authentication.ForgotPasswordRequest;
import studentConsulting.model.payload.request.authentication.LoginRequest;
import studentConsulting.model.payload.request.authentication.RegisterRequest;
import studentConsulting.model.payload.request.authentication.ResendVerificationRequest;
import studentConsulting.model.payload.request.authentication.ResetPasswordRequest;
import studentConsulting.model.payload.request.authentication.UpdateInformationRequest;
import studentConsulting.model.payload.request.authentication.VerifyCodeCheckRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.address.AddressRepository;
import studentConsulting.repository.address.DistrictRepository;
import studentConsulting.repository.address.ProvinceRepository;
import studentConsulting.repository.address.WardRepository;
import studentConsulting.repository.authentication.AccountRepository;
import studentConsulting.repository.authentication.RoleAuthRepository;
import studentConsulting.repository.authentication.RoleRepository;
import studentConsulting.repository.authentication.UserRepository;
import studentConsulting.security.JWT.JwtProvider;
import studentConsulting.service.implement.address.AddressServiceImpl;
import studentConsulting.service.implement.email.EmailServiceImpl;
import studentConsulting.service.interfaces.authentication.IUserService;
import studentConsulting.util.RandomUtils;

@Service
public class UserServiceImpl implements IUserService {
	private final long expireInRefresh = Duration.ofHours(10).toMillis();
    @Autowired
    RoleRepository roleRepository;

    @Autowired
    AccountRepository accountRepository;
    @Autowired
    AddressRepository addressRepository;

    @Autowired
    UserRepository userRepository;

    @Autowired
    PasswordEncoder passwordEncoder;

    @Autowired
    private JwtProvider jwtProvider;

    @Autowired
    private RoleAuthRepository tokenRepository;

    @Autowired
    private JavaMailSender javaMailSender;

    @Autowired
    private EmailServiceImpl emailService;
    
    @Autowired
    private ProvinceRepository provinceRepository;
    
    @Autowired
    private DistrictRepository districtRepository;
    
    @Autowired
    private WardRepository wardRepository;

    @Autowired
    private AddressServiceImpl addressService;
    
    // build token
    // Tạo ra và lưu trữ một token mới, sau đó trả về thông tin phản hồi đăng nhập bao gồm token truy cập,
    // thời gian hết hạn, và mã định danh token để người dùng có thể sử dụng trong các yêu cầu tiếp theo.
	/*
	 * Xây dựng Token
	 */    
    private DataResponse<DataResponse.LoginData> buildToken(UserInformationEntity userModel) {
        try {
            String jti = UUID.randomUUID().toString();
            long expiredTime = System.currentTimeMillis() + expireInRefresh;

            // Log values
            System.out.println("Generating token with ID: " + jti);
            System.out.println("Token expires at: " + expiredTime);

            // Save the token to the repository
            tokenRepository.save(RoleAuthEntity.builder()
                    .user(userModel) // Adjust this if necessary
                    .tokenId(jti)
                    .expiredTime(expiredTime)
                    .build());

            // Generate the access token
            String accessToken = jwtProvider.createToken(userModel); // Adjust this if necessary

            // Create UserInformationDTO
            UserInformationDTO userDto = UserInformationDTO.builder()
                    .id(userModel.getId())
                    .studentCode(userModel.getStudentCode())
                    .schoolName(userModel.getSchoolName())
                    .firstName(userModel.getFirstName())
                    .lastName(userModel.getLastName())
                    .phone(userModel.getPhone())
                    .avatarUrl(userModel.getAvatarUrl())
                    .gender(userModel.getGender())
                    .build();

            // Build and return the DataResponse with nested data
            DataResponse.LoginData loginData = DataResponse.LoginData.builder()
                    .user(userDto)
                    .accessToken(accessToken)
                    .expiresIn(expiredTime)
                    .refreshToken(jti)
                    .build();

            return DataResponse.<DataResponse.LoginData>builder()
                    .status("success")
                    .message("Login successful")
                    .data(loginData)
                    .build();
        } catch (Exception e) {
            // Log exception
            System.err.println("Error building token: " + e.getMessage());
            e.printStackTrace();
            throw new RuntimeException("Error building token", e);
        }
    }




	/*
	 * Xử lý Refresh Token
	 */
    @Override
    public DataResponse<DataResponse.LoginData> refreshToken(String refreshToken) {
        RoleAuthEntity tokenModel = getValidToken(refreshToken);
        Optional<UserInformationEntity> userModel = userRepository.findById(tokenModel.getUser().getId());
        return buildToken(userModel.get());  // Correctly returning DataResponse<LoginData>
    }



    private RoleAuthEntity getValidToken(String refreshToken) {
        RoleAuthEntity tokenModel = tokenRepository.findByTokenId(refreshToken);
        if (tokenModel == null || tokenModel.getId() <= 0) {
            throw new ErrorException("Mã refresh token không tồn tại");
        } else if (System.currentTimeMillis() > tokenModel.getExpiredTime()) {
            throw new ErrorException("Mã refresh token đã hết hạn lúc " + new DateTime(tokenModel.getExpiredTime()));
        }
        return tokenModel;
    }
	/*
	 * Đăng ký Tài Khoản
	 */    
    String urlConfirm;
    @Override
    public DataResponse<UserInformationDTO> register(RegisterRequest registerRequest) {
        validateRegistrationFields(registerRequest);
    	checkAccountExistence(registerRequest);
        String verifyTokens = RandomUtils.getRandomVerifyCode();
        urlConfirm = verifyTokens;

        // Create and save the account first
        AccountEntity accountModel = createAccount(registerRequest, verifyTokens);
        accountRepository.save(accountModel); // Save the accountModel to the database

        // Create the user with the saved account
        UserInformationEntity userModel = createUser(registerRequest, accountModel);
        userRepository.save(userModel);
        
        // Send registration email
        sendRegistrationEmail(registerRequest.getEmail(), verifyTokens, accountModel);

        // Create AccountDTO from AccountEntity
        AccountDTO accountDto = AccountDTO.builder()
                .id(accountModel.getId())
                .username(accountModel.getUsername())
                .email(accountModel.getEmail())
                .isActivity(accountModel.isActivity())
                .verifyRegister(accountModel.getVerifyRegister())
                .build();

        // Create UserInformationDTO from UserInformationEntity
        UserInformationDTO userDto = UserInformationDTO.builder()
                .id(userModel.getId())
                .studentCode(userModel.getStudentCode())
                .schoolName(userModel.getSchoolName())
                .firstName(userModel.getFirstName())
                .lastName(userModel.getLastName())
                .phone(userModel.getPhone())
                .avatarUrl(userModel.getAvatarUrl())
                .gender(userModel.getGender())
                .account(accountDto)
                .build();

        // Return DataResponse with UserInformationDTO
        return DataResponse.<UserInformationDTO>builder()
                .status("success")
                .message("Đăng ký thành công! Vui lòng kiểm tra email để xác nhận đăng ký.")
                .data(userDto)
                .build();
    }



    private void checkAccountExistence(RegisterRequest registerRequest) {
        AccountEntity existingAccount = accountRepository.findAccountByUsername(registerRequest.getUsername());
        if (existingAccount != null && existingAccount.getId() >= 0) {
            throw new ErrorException("Tài khoản đã tồn tại. Vui lòng nhập lại!");
        }

        if (accountRepository.existsByEmail(registerRequest.getEmail())) {
            throw new ErrorException("Email đã tồn tại. Vui lòng nhập lại!");
        }

        if (userRepository.existsByPhone(registerRequest.getPhone())) {
            throw new ErrorException("Số điện thoại đã tồn tại. Vui lòng nhập lại!");
        }
    }

    private AccountEntity createAccount(RegisterRequest registerRequest, String verifyTokens) {
        RoleEntity roleModel = roleRepository.findByName(SecurityConstants.Role.USER);
        if (roleModel == null) {
            throw new ResourceNotFoundException(ResourceName.RoleEntity, FieldName.NAME, SecurityConstants.Role.USER);
        }

        String hashedPassword = passwordEncoder.encode(registerRequest.getPassword());

        return AccountEntity.builder()
                .username(registerRequest.getUsername())
                .role(roleModel)
                .email(registerRequest.getEmail())
                .password(hashedPassword)
                .isActivity(false)
                .verifyRegister(verifyTokens)
                .createdAt(Timestamp.valueOf(LocalDateTime.now()))
                .updatedAt(Timestamp.valueOf(LocalDateTime.now()))
                .build();
    }

    private UserInformationEntity createUser(RegisterRequest registerRequest, AccountEntity accountModel) {
        return UserInformationEntity.builder()
                .phone(registerRequest.getPhone())
                .gender(registerRequest.getGender())
                .account(accountModel)
                .createdAt(Timestamp.valueOf(LocalDateTime.now()))
                .updatedAt(Timestamp.valueOf(LocalDateTime.now()))
                .build();
    }

    private void validateRegistrationFields(RegisterRequest registerRequest) {
        if (!isValidEmail(registerRequest.getEmail())) {
            throw new ErrorException("Email không hợp lệ! Vui lòng nhập đúng định dạng email.");
        }

        if (!isStrongPassword(registerRequest.getPassword())) {
            throw new ErrorException("Mật khẩu phải chứa ít nhất 12 ký tự, bao gồm chữ hoa, chữ thường, số và ký tự đặc biệt.");
        }

        if (!isValidPhoneNumber(registerRequest.getPhone())) {
            throw new ErrorException("Số điện thoại không hợp lệ! Số điện thoại chỉ có 10 số và không chứa ký tự chữ cái.");
        }

        if (!registerRequest.getPassword().equals(registerRequest.getConfirmPassword())) {
            throw new ErrorException("Mật khẩu và xác nhận mật khẩu không khớp.");
        }
        if (!isValidGender(registerRequest.getGender())) {
            throw new ErrorException("Giới tính không hợp lệ! Chỉ chấp nhận giá trị 'NAM' hoặc 'NU'.");
        }
        if (!registerRequest.getUsername().matches("^[a-zA-Z]+$")) {
            throw new ErrorException("Tên người dùng chỉ được chứa các chữ cái.");
        }

    }
    

    private boolean isValidGender(String gender) {
        return "NAM".equalsIgnoreCase(gender) || "NU".equalsIgnoreCase(gender);
    }
    
    private boolean isStrongPassword(String password) {
        String passwordRegex = "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[@$!%*?&#])[A-Za-z\\d@$!%*?&#]{12,}$";
        Pattern pattern = Pattern.compile(passwordRegex);
        return pattern.matcher(password).matches();
    }

    private boolean isValidPhoneNumber(String phoneNumber) {
        String phoneRegex = "^[0-9]{10}$";
        Pattern pattern = Pattern.compile(phoneRegex);
        return pattern.matcher(phoneNumber).matches();
    }
    
    private void sendRegistrationEmail(String email, String verifyTokens, AccountEntity account) {
        try {
            MimeMessage mailMessage = javaMailSender.createMimeMessage();
            MimeMessageHelper mailHelper = new MimeMessageHelper(mailMessage, true, "UTF-8");
            mailHelper.setFrom("ngoquangnghia111003@gmail.com");
            mailHelper.setTo(email);
            mailHelper.setSubject("Xác nhận đăng ký tài khoản");
            mailHelper.setText(
                body + "<div class=\"text\" style=\"padding: 0 2.5em; text-align: center;\">\r\n"
                    + "    <h3>Cảm ơn bạn đã đăng ký tài khoản\r\n"
                    + "</h3>\r\n"
                    + "    <h4>Vui lòng nhấp vào liên kết dưới đây để xác nhận đăng ký tài khoản:\r\n"
                    + "</h4>\r\n"
                    + "    <p>" + verifyTokens + "</p>\r\n"
                    + "</div>\r\n"
                    + footer, true);
            javaMailSender.send(mailMessage);

            // Cập nhật thời gian hết hạn và số lần thử mã xác nhận
            account.setVerifyRegister(verifyTokens);
            account.setVerifyCodeExpirationTime(new Timestamp(System.currentTimeMillis() + 5 * 60 * 1000)); // 5 phút
            account.setVerifyCodeAttemptCount(0);
            accountRepository.save(account);
        } catch (MessagingException e) {
            throw new ErrorException("Lỗi gửi email xác nhận");
        }
    }


	/*
	 * Xác Nhận Đăng Ký
	 */   
    @Override
    public DataResponse<Object> confirmRegistration(ConfirmRegistrationRequest confirmRegistrationRequest) {
        AccountEntity account = accountRepository.findAccountByEmail(confirmRegistrationRequest.getEmailRequest());

        if (account == null || account.getId() <= 0) {
            throw new ResourceNotFoundException(ResourceName.AccountEntity, FieldName.USERNAME, confirmRegistrationRequest.getEmailRequest());
        }

        // Kiểm tra mã xác nhận đã hết hạn chưa
        if (System.currentTimeMillis() > account.getVerifyCodeExpirationTime().getTime()) {
            throw new ErrorException("Mã xác nhận đã hết hạn!");
        }

        // Kiểm tra số lần nhập mã
        if (account.getVerifyCodeAttemptCount() >= 3) {
            throw new ErrorException("Bạn đã nhập sai mã xác nhận quá số lần cho phép. Mã xác nhận đã bị vô hiệu.");
        }

        // Kiểm tra mã xác nhận
        if (!account.getVerifyRegister().equals(confirmRegistrationRequest.getToken())) {
            account.setVerifyCodeAttemptCount(account.getVerifyCodeAttemptCount() + 1);
            accountRepository.save(account);
            throw new ErrorException("Mã xác thực không đúng. Vui lòng kiểm tra lại!");
        }

        // Mã xác nhận đúng, kích hoạt tài khoản và reset số lần nhập
        account.setActivity(true);
        account.setVerifyRegister(null);
        account.setVerifyCodeAttemptCount(0);
        account.setVerifyCodeExpirationTime(null);
        accountRepository.save(account);

        return DataResponse.builder().status("success").message("Xác nhận thành công!").build();
    }

	/*
	 * Đăng Nhập
	 */    
    @Override
    public DataResponse<DataResponse.LoginData> login(LoginRequest loginRequest) {
        if (!isValidEmail(loginRequest.getEmail())) {
            throw new ErrorException("Email không hợp lệ! Vui lòng nhập đúng định dạng email.");
        }
        AccountEntity accountModel = accountRepository.findAccountByEmail(loginRequest.getEmail());
        if (accountModel == null) {
            throw new ErrorException("Email không tồn tại trong hệ thống.");
        }
        if (!accountModel.isActivity()) {
            throw new ErrorException("Tài khoản đã bị khóa! Vui lòng liên hệ với quản trị viên.");
        }
        if (!passwordEncoder.matches(loginRequest.getPassword(), accountModel.getPassword())) {
            throw new ErrorException("Mật khẩu không chính xác! Vui lòng thử lại.");
        }
        return buildToken(userRepository.findUserInfoModelByAccountModel(accountModel));
    }

    private boolean isValidEmail(String email) {
        String emailRegex = "^[a-zA-Z0-9_+&*-]+(?:\\.[a-zA-Z0-9_+&*-]+)*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,7}$";
        Pattern pattern = Pattern.compile(emailRegex);
        if (email == null) {
            return false;
        }
        return pattern.matcher(email).matches();
    }

   
	/*
	 * Thay Đổi Mật Khẩu
	 */    
    @Transactional
    @Override
    public DataResponse<Object> changePassword(String username, ChangePasswordRequest changePasswordRequest) {
        AccountEntity account = accountRepository.findAccountWithRolesByUsername(username);

        
        if (account == null) {
            throw new ErrorException("Tài khoản không tồn tại");
        }

        if (!passwordEncoder.matches(changePasswordRequest.getPassword(), account.getPassword())) {
            throw new ErrorException("Nhập sai mật khẩu cũ");
        }

        if (passwordEncoder.matches(changePasswordRequest.getNewPassword(), account.getPassword())) {
            throw new ErrorException("Mật khẩu mới không được trùng với mật khẩu cũ");
        }

        if (!isStrongPassword(changePasswordRequest.getNewPassword())) {
            throw new ErrorException("Mật khẩu mới phải chứa ít nhất 12 ký tự, bao gồm chữ hoa, chữ thường, số và ký tự đặc biệt");
        }

        String hashedPassword = passwordEncoder.encode(changePasswordRequest.getNewPassword());
        account.setPassword(hashedPassword);
        accountRepository.save(account);

        return DataResponse.builder()
                .status("success")
                .message("Thay đổi mật khẩu thành công")
                .build();
    }
	/*
	 * Quên Mật Khẩu
	 */    
    @Override
    public DataResponse<Object> forgotPassword(ForgotPasswordRequest forgotPasswordRequest) {
        // Kiểm tra email có hợp lệ hay không
        if (!isValidEmail(forgotPasswordRequest.getEmailRequest())) {
            throw new ErrorException("Email không hợp lệ! Vui lòng nhập đúng định dạng email.");
        }

        // Tìm kiếm tài khoản dựa trên email
        AccountEntity account = accountRepository.findAccountByEmail(forgotPasswordRequest.getEmailRequest());
        if (account == null) {
            throw new ErrorException("Email không tồn tại trong hệ thống. Vui lòng kiểm tra lại.");
        }

        // Kiểm tra xem tài khoản có đang hoạt động không
        if (!account.isActivity()) {
            throw new ErrorException("Tài khoản đã bị khóa! Không thể yêu cầu đặt lại mật khẩu.");
        }

        // Tạo mã xác nhận mới và gửi email
        String verifyCode = RandomUtils.getRandomVerifyCode();
        sendForgotPasswordEmail(forgotPasswordRequest.getEmailRequest(), verifyCode, account);

        return DataResponse.builder()
                .status("success")
                .message("Mã xác nhận đã được gửi qua email!")
                .build();
    }



 


    private void sendForgotPasswordEmail(String email, String verifyCode, AccountEntity account) {
        try {
            MimeMessage mailMessage = javaMailSender.createMimeMessage();
            MimeMessageHelper mailHelper = new MimeMessageHelper(mailMessage, true, "UTF-8");
            mailHelper.setFrom("ngoquangnghia111003@gmail.com");
            mailHelper.setTo(email);
            mailHelper.setSubject("Mã xác nhận lấy lại mật khẩu");
            mailHelper.setText(
                body + "<div class=\"text\" style=\"padding: 0 2.5em; text-align: center;\">\r\n"
                    + "    <h3>Bạn vừa yêu cầu cập nhật lại mật khẩu</h3>\r\n"
                    + "    <h4>Đây là mã xác nhận lấy lại mật khẩu của bạn</h4>\r\n"
                    + "    <p>" + verifyCode + "</p>\r\n"
                    + "</div>\r\n"
                    + footer, true);
            emailService.sendEmail(mailMessage);

            // Cập nhật thời gian hết hạn và số lần gửi mã xác nhận
            account.setVerifyCode(verifyCode);
            account.setVerifyCodeExpirationTime(new Timestamp(System.currentTimeMillis() + 5 * 60 * 1000)); // 5 phút
            account.setVerifyCodeAttemptCount(0);
            accountRepository.save(account);
        } catch (Exception e) {
            throw new ErrorException("Lỗi gửi mã xác nhận!");
        }
    }


	/*
	 * Kiểm Tra Mã Xác Nhận
	 */
    @Override
    public DataResponse<Object> checkVerifyCode(VerifyCodeCheckRequest verifyCode) {
        AccountEntity account = accountRepository.findAccountByEmail(verifyCode.getEmailRequest());
        if (account == null || account.getId() <= 0) {
            throw new ResourceNotFoundException(ResourceName.AccountEntity, FieldName.USERNAME, verifyCode.getEmailRequest());
        }

        // Kiểm tra mã xác nhận đã hết hạn chưa
        if (System.currentTimeMillis() > account.getVerifyCodeExpirationTime().getTime()) {
            throw new ErrorException("Mã xác nhận đã hết hạn!");
        }

        // Kiểm tra số lần nhập mã
        if (account.getVerifyCodeAttemptCount() >= 3) {
            throw new ErrorException("Bạn đã nhập sai mã xác nhận quá số lần cho phép. Mã xác nhận đã bị vô hiệu.");
        }

        // Kiểm tra mã xác nhận
        if (!account.getVerifyCode().equals(verifyCode.getCode())) {
            account.setVerifyCodeAttemptCount(account.getVerifyCodeAttemptCount() + 1);
            accountRepository.save(account);
            throw new ErrorException("Sai mã xác thực!");
        }

        // Mã xác nhận đúng, reset số lần nhập và xoá mã xác nhận
        account.setVerifyCodeAttemptCount(0);
        account.setVerifyCode(null);
        account.setVerifyCodeExpirationTime(null);
        accountRepository.save(account);

        return DataResponse.builder().status("success").message("Xác thực mã thành công!").build();
    }

	/*
	 * Đặt Lại Mật Khẩu
	 */
    @Override
    public DataResponse<Object> resetPassword(ResetPasswordRequest resetPasswordRequest) {
        AccountEntity account = accountRepository.findAccountByEmail(resetPasswordRequest.getEmail());
        if (account == null || account.getId() <= 0) {
            throw new ResourceNotFoundException(ResourceName.AccountEntity, FieldName.USERNAME, resetPasswordRequest.getEmail());
        }
        if (!account.isActivity()) {
            throw new ErrorException("Tài khoản đã bị khóa! Vui lòng liên hệ với quản trị viên.");
        }
        if (!isStrongPassword(resetPasswordRequest.getNewPassword())) {
            throw new ErrorException("Mật khẩu mới không đáp ứng các yêu cầu bảo mật(phải có chữ hoa, thường, số, ký tự và trên 12 ký tự).");
        }

        String hashedPassword = passwordEncoder.encode(resetPasswordRequest.getNewPassword());
        account.setPassword(hashedPassword);
        account.setVerifyCode(null); // Clear the verification code after successful reset
        accountRepository.save(account);

        return DataResponse.builder().status("success").message("Cập nhật mật khẩu thành công!").build();
    }
    
    
    
    /*
	 * Gửi lại mã xác nhận khi quá hạn hoặc quá số lần
	 */    
    public DataResponse<Object> resendVerificationCodeForRegister(ResendVerificationRequest resendRequest) {
        if (!isValidEmail(resendRequest.getEmailRequest())) {
            throw new ErrorException("Email không hợp lệ! Vui lòng nhập đúng định dạng email.");
        }

        AccountEntity account = accountRepository.findAccountByEmail(resendRequest.getEmailRequest());
        if (account == null) {
            throw new ErrorException("Email không tồn tại trong hệ thống.");
        }

        // Kiểm tra nếu mã đã bị vô hiệu hóa do nhập sai quá nhiều lần hoặc đã hết hạn
        boolean isCodeExpired = System.currentTimeMillis() > account.getVerifyCodeExpirationTime().getTime();

        if (account.getVerifyCodeAttemptCount() >= 3 || isCodeExpired) {
            String newVerifyCode = RandomUtils.getRandomVerifyCode();
            sendRegistrationEmail(account.getEmail(), newVerifyCode, account); // Gửi mã xác nhận mới cho đăng ký
            
            // Cập nhật lại thông tin mã xác nhận mới
            account.setVerifyRegister(newVerifyCode);
            account.setVerifyCodeExpirationTime(new Timestamp(System.currentTimeMillis() + 5 * 60 * 1000)); // 5 phút
            account.setVerifyCodeAttemptCount(0); // Reset lại số lần nhập mã xác nhận
            accountRepository.save(account);
            
            return DataResponse.builder()
                    .status("success")
                    .message("Mã xác nhận mới cho đăng ký đã được gửi lại!")
                    .build();
        } else {
            throw new ErrorException("Mã xác nhận hiện tại vẫn còn hiệu lực.");
        }
    }

    
    public DataResponse<Object> resendVerificationCodeForForgotPassword(ResendVerificationRequest resendRequest) {
        
    	if (!isValidEmail(resendRequest.getEmailRequest())) {
            throw new ErrorException("Email không hợp lệ! Vui lòng nhập đúng định dạng email.");
        }

        AccountEntity account = accountRepository.findAccountByEmail(resendRequest.getEmailRequest());
        if (account == null) {
            throw new ErrorException("Email không tồn tại trong hệ thống.");
        }

        // Kiểm tra nếu mã đã bị vô hiệu hóa do nhập sai quá nhiều lần hoặc đã hết hạn
        boolean isCodeExpired = System.currentTimeMillis() > account.getVerifyCodeExpirationTime().getTime();
        
        if (account.getVerifyCodeAttemptCount() >= 3 || isCodeExpired) {
            String newVerifyCode = RandomUtils.getRandomVerifyCode();
            sendForgotPasswordEmail(account.getEmail(), newVerifyCode, account); // Gửi mã xác nhận mới cho quên mật khẩu
            
            // Cập nhật lại thông tin mã xác nhận mới
            account.setVerifyCode(newVerifyCode);
            account.setVerifyCodeExpirationTime(new Timestamp(System.currentTimeMillis() + 5 * 60 * 1000)); // 5 phút
            account.setVerifyCodeAttemptCount(0); // Reset lại số lần nhập mã xác nhận
            accountRepository.save(account);
            
            return DataResponse.builder()
                    .status("success")
                    .message("Mã xác nhận mới cho quên mật khẩu đã được gửi lại!")
                    .build();
        } else {
            throw new ErrorException("Mã xác nhận hiện tại vẫn còn hiệu lực.");
        }
    }



    
    /*
	 * Thay đổi email khi đợi mã xác nhận
	 */    
    public DataResponse<Object> changeEmail(ChangeEmailRequest changeEmailRequest) {
        // Tìm tài khoản dựa trên email cũ
        AccountEntity account = accountRepository.findAccountByEmail(changeEmailRequest.getOldEmail());
        if (account == null) {
            throw new ErrorException("Email cũ không tồn tại trong hệ thống.");
        }

        // Kiểm tra email mới không được trùng với email cũ
        if (changeEmailRequest.getNewEmail().equalsIgnoreCase(changeEmailRequest.getOldEmail())) {
            throw new ErrorException("Email mới không được trùng với email cũ. Vui lòng nhập email khác.");
        }

        // Kiểm tra email mới có hợp lệ không
        if (!isValidEmail(changeEmailRequest.getNewEmail())) {
            throw new ErrorException("Email mới không hợp lệ! Vui lòng nhập đúng định dạng email.");
        }
        
        if (!isValidEmail(changeEmailRequest.getOldEmail())) {
            throw new ErrorException("Email cũ không hợp lệ! Vui lòng nhập đúng định dạng email.");
        }

        // Cập nhật email mới
        account.setEmail(changeEmailRequest.getNewEmail());
        account.setVerifyRegister(RandomUtils.getRandomVerifyCode());
        accountRepository.save(account);

        // Gửi mã xác nhận mới tới email mới
        sendRegistrationEmail(changeEmailRequest.getNewEmail(), account.getVerifyRegister(), account);

        return DataResponse.builder()
                .status("success")
                .message("Email đã được cập nhật và mã xác nhận mới đã được gửi.")
                .build();
    }
    
	/*
	 * Quản Lý Người Dùng
	 */    
    @Override
    public Iterable<UserInformationEntity> getAllUser() {
        return userRepository.findAllByRoleName("USER");
    }

    @Override
    public UserInformationDTO getProfile(Integer currentUserId) {
        try {
            Optional<UserInformationEntity> userInformation = userRepository.findById(currentUserId);

            // Kiểm tra nếu không tìm thấy thông tin người dùng
            if (!userInformation.isPresent()) {
                throw new ResourceNotFoundException(ResourceName.UserInformationEntity, FieldName.ID, currentUserId);
            }

            UserInformationEntity userEntity = userInformation.get();

            // Chuyển đổi entity thành DTO, bao gồm cả thông tin địa chỉ
            UserInformationDTO.UserInformationDTOBuilder userDtoBuilder = UserInformationDTO.builder()
                    .id(userEntity.getId())
                    .studentCode(userEntity.getStudentCode())
                    .schoolName(userEntity.getSchoolName())
                    .firstName(userEntity.getFirstName())
                    .lastName(userEntity.getLastName())
                    .phone(userEntity.getPhone())
                    .avatarUrl(userEntity.getAvatarUrl())
                    .gender(userEntity.getGender())
            		.email(userEntity.getAccount().getEmail());

            // Kiểm tra và chuyển đổi thông tin địa chỉ nếu tồn tại
            if (userEntity.getAddress() != null) {
            	// Lấy thông tin tên từ mã code
                String line = userEntity.getAddress().getLine();
                String provinceName = userEntity.getAddress().getProvince().getName();
                String districtName = userEntity.getAddress().getDistrict().getName();
                String wardName = userEntity.getAddress().getWard().getName();

                // Ghi ra console
                System.out.println("Line: " + line);
                System.out.println("Province: " + provinceName);
                System.out.println("District: " + districtName);
                System.out.println("Ward: " + wardName);
                
                AddressDTO addressDto = AddressDTO.builder()
                        .line(userEntity.getAddress().getLine())
                        .provinceCode(userEntity.getAddress().getProvince().getCode())
                        .districtCode(userEntity.getAddress().getDistrict().getCode())
                        .wardCode(userEntity.getAddress().getWard().getCode())
                        .build();

                userDtoBuilder.address(addressDto);
            }

            // Trả về UserInformationDTO đã xây dựng
            return userDtoBuilder.build();
            
        } catch (Exception e) {
            // Ném ngoại lệ để xử lý ở controller
            throw new RuntimeException("Đã xảy ra lỗi hệ thống", e);
        }
    }

    public DataResponse<Object> updateProfile(Integer userId, UpdateInformationRequest userUpdateRequest) {
        // Tìm người dùng hiện tại theo ID
        UserInformationEntity userEntity = userRepository.findById(userId)
            .orElseThrow(() -> new ResourceNotFoundException("User", "id", userId));
        System.out.println("Current User Information:");
        System.out.println("Student Code: " + userEntity.getStudentCode());
        System.out.println("School Name: " + userEntity.getSchoolName());
        System.out.println("First Name: " + userEntity.getFirstName());
        System.out.println("Last Name: " + userEntity.getLastName());
        System.out.println("Phone: " + userEntity.getPhone());
        System.out.println("Avatar URL: " + userEntity.getAvatarUrl());
        System.out.println("Gender: " + userEntity.getGender());
        System.out.println("Email: " + userEntity.getAccount().getEmail());

        
        if (userEntity.getAddress() != null) {
            System.out.println("Address Line: " + userEntity.getAddress().getLine());
            System.out.println("Province: " + userEntity.getAddress().getProvince().getName());
            System.out.println("District: " + userEntity.getAddress().getDistrict().getName());
            System.out.println("Ward: " + userEntity.getAddress().getWard().getName());
        }
        
        // Kiểm tra và cập nhật thông tin cá nhân
        if (!userEntity.getStudentCode().equals(userUpdateRequest.getStudentCode())) {
            if (userRepository.existsByStudentCode(userUpdateRequest.getStudentCode())) {
                throw new ErrorException("Mã sinh viên đã tồn tại.");
            }
        }

        if (!userEntity.getPhone().equals(userUpdateRequest.getPhone())) {
            if (userRepository.existsByPhone(userUpdateRequest.getPhone())) {
                throw new ErrorException("Số điện thoại đã tồn tại.");
            }
        }

        if (!userEntity.getAccount().getEmail().equals(userUpdateRequest.getEmail())) {
            if (userRepository.existsByAccount_Email(userUpdateRequest.getEmail())) {
                throw new ErrorException("Email đã tồn tại.");
            }
        }
        if (!isValidPhoneNumber(userUpdateRequest.getPhone())) {
            throw new ErrorException("Số điện thoại không hợp lệ! Số điện thoại chỉ có 10 số và không chứa ký tự chữ cái.");
        }
        
        if (!isValidGender(userUpdateRequest.getGender())) {
            throw new ErrorException("Giới tính không hợp lệ, chỉ chấp nhận 'NAM' hoặc 'NỮ'.");
        }
        
        if (!isValidEmail(userUpdateRequest.getEmail())) {
            throw new ErrorException("Email không hợp lệ! Vui lòng nhập đúng định dạng email.");
        }
        userEntity.setStudentCode(userUpdateRequest.getStudentCode());
        userEntity.setSchoolName(userUpdateRequest.getSchoolName());
        userEntity.setFirstName(userUpdateRequest.getFirstName());
        userEntity.setLastName(userUpdateRequest.getLastName());
        userEntity.setPhone(userUpdateRequest.getPhone());
        userEntity.setAvatarUrl(userUpdateRequest.getAvatarUrl());
        userEntity.setGender(userUpdateRequest.getGender());

        if (!userEntity.getAccount().getEmail().equals(userUpdateRequest.getEmail())) {
            if (accountRepository.existsByEmail(userUpdateRequest.getEmail())) {
                throw new ErrorException("Email đã tồn tại.");
            }
            userEntity.getAccount().setEmail(userUpdateRequest.getEmail());
        }

        
        if (userUpdateRequest.getAddress() != null) {
            addressService.updateAddress(userEntity, userUpdateRequest.getAddress());
        }

        userRepository.save(userEntity);

        

        AddressDTO addressDto = AddressDTO.builder()
                .line(userEntity.getAddress().getLine())
                .provinceCode(userEntity.getAddress().getProvince().getCode())
                .districtCode(userEntity.getAddress().getDistrict().getCode())
                .wardCode(userEntity.getAddress().getWard().getCode())
                .build();

        UserInformationDTO userDto = UserInformationDTO.builder()
                .id(userEntity.getId())
                .studentCode(userEntity.getStudentCode())
                .schoolName(userEntity.getSchoolName())
                .firstName(userEntity.getFirstName())
                .lastName(userEntity.getLastName())
                .phone(userEntity.getPhone())
                .avatarUrl(userEntity.getAvatarUrl())
                .gender(userEntity.getGender())
        		.email(userEntity.getAccount().getEmail())
                .address(addressDto)
                .build();

        return DataResponse.builder()
                .status("success")
                .message("Cập nhật hồ sơ thành công")
                .data(userDto)
                .build();
    }



    @Override
    public Integer getUserIdByUsername(String username) {
        // Tìm người dùng dựa trên username trong repository
        UserInformationEntity userEntity = userRepository.findByAccount_Username(username)
            .orElseThrow(() -> new ResourceNotFoundException("User", "username", username));

        // Trả về userId
        return userEntity.getId();
    }

    
    
    String body = "<!DOCTYPE html>\r\n"
			+ "<html lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:v=\"urn:schemas-microsoft-com:vml\" xmlns:o=\"urn:schemas-microsoft-com:office:office\">\r\n"
			+ "<head>\r\n" + "    <meta charset=\"utf-8\"> <!-- utf-8 works for most cases -->\r\n"
			+ "    <meta name=\"viewport\" content=\"width=device-width\"> <!-- Forcing initial-scale shouldn't be necessary -->\r\n"
			+ "    <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"> <!-- Use the latest (edge) version of IE rendering engine -->\r\n"
			+ "    <meta name=\"x-apple-disable-message-reformatting\">  <!-- Disable auto-scale in iOS 10 Mail entirely -->\r\n"
			+ "    <title></title> <!-- The title tag shows in email notifications, like Android 4.4. -->\r\n" + "\r\n"
			+ "    <link href=\"https://fonts.googleapis.com/css?family=Lato:300,400,700\" rel=\"stylesheet\">\r\n"
			+ "\r\n" + "    <!-- CSS Reset : BEGIN -->\r\n" + "    <style>\r\n" + "\r\n"
			+ "        /* What it does: Remove spaces around the email design added by some email clients. */\r\n"
			+ "        /* Beware: It can remove the padding / margin and add a background color to the compose a reply window. */\r\n"
			+ "        html,\r\n" + "body {\r\n" + "    margin: 0 auto !important;\r\n"
			+ "    padding: 0 !important;\r\n" + "    height: 100% !important;\r\n" + "    width: 100% !important;\r\n"
			+ "    background: #f1f1f1;\r\n" + "}\r\n" + "\r\n"
			+ "/* What it does: Stops email clients resizing small text. */\r\n" + "* {\r\n"
			+ "    -ms-text-size-adjust: 100%;\r\n" + "    -webkit-text-size-adjust: 100%;\r\n" + "}\r\n" + "\r\n"
			+ "/* What it does: Centers email on Android 4.4 */\r\n" + "div[style*=\"margin: 16px 0\"] {\r\n"
			+ "    margin: 0 !important;\r\n" + "}\r\n" + "\r\n"
			+ "/* What it does: Stops Outlook from adding extra spacing to tables. */\r\n" + "table,\r\n" + "td {\r\n"
			+ "    mso-table-lspace: 0pt !important;\r\n" + "    mso-table-rspace: 0pt !important;\r\n" + "}\r\n"
			+ "\r\n" + "/* What it does: Fixes webkit padding issue. */\r\n" + "table {\r\n"
			+ "    border-spacing: 0 !important;\r\n" + "    border-collapse: collapse !important;\r\n"
			+ "    table-layout: fixed !important;\r\n" + "    margin: 0 auto !important;\r\n" + "}\r\n" + "\r\n"
			+ "/* What it does: Uses a better rendering method when resizing images in IE. */\r\n" + "img {\r\n"
			+ "    -ms-interpolation-mode:bicubic;\r\n" + "}\r\n" + "\r\n"
			+ "/* What it does: Prevents Windows 10 Mail from underlining links despite inline CSS. Styles for underlined links should be inline. */\r\n"
			+ "a {\r\n" + "    text-decoration: none;\r\n" + "}\r\n" + "\r\n"
			+ "/* What it does: A work-around for email clients meddling in triggered links. */\r\n"
			+ "*[x-apple-data-detectors],  /* iOS */\r\n" + ".unstyle-auto-detected-links *,\r\n" + ".aBn {\r\n"
			+ "    border-bottom: 0 !important;\r\n" + "    cursor: default !important;\r\n"
			+ "    color: inherit !important;\r\n" + "    text-decoration: none !important;\r\n"
			+ "    font-size: inherit !important;\r\n" + "    font-family: inherit !important;\r\n"
			+ "    font-weight: inherit !important;\r\n" + "    line-height: inherit !important;\r\n" + "}\r\n" + "\r\n"
			+ "/* What it does: Prevents Gmail from displaying a download button on large, non-linked images. */\r\n"
			+ ".a6S {\r\n" + "    display: none !important;\r\n" + "    opacity: 0.01 !important;\r\n" + "}\r\n"
			+ "\r\n" + "/* What it does: Prevents Gmail from changing the text color in conversation threads. */\r\n"
			+ ".im {\r\n" + "    color: inherit !important;\r\n" + "}\r\n" + "\r\n"
			+ "/* If the above doesn't work, add a .g-img class to any image in question. */\r\n"
			+ "img.g-img + div {\r\n" + "    display: none !important;\r\n" + "}\r\n" + "\r\n"
			+ "/* What it does: Removes right gutter in Gmail iOS app: https://github.com/TedGoas/Cerberus/issues/89  */\r\n"
			+ "/* Create one of these media queries for each additional viewport size you'd like to fix */\r\n" + "\r\n"
			+ "/* iPhone 4, 4S, 5, 5S, 5C, and 5SE */\r\n"
			+ "@media only screen and (min-device-width: 320px) and (max-device-width: 374px) {\r\n"
			+ "    u ~ div .email-container {\r\n" + "        min-width: 320px !important;\r\n" + "    }\r\n" + "}\r\n"
			+ "/* iPhone 6, 6S, 7, 8, and X */\r\n"
			+ "@media only screen and (min-device-width: 375px) and (max-device-width: 413px) {\r\n"
			+ "    u ~ div .email-container {\r\n" + "        min-width: 375px !important;\r\n" + "    }\r\n" + "}\r\n"
			+ "/* iPhone 6+, 7+, and 8+ */\r\n" + "@media only screen and (min-device-width: 414px) {\r\n"
			+ "    u ~ div .email-container {\r\n" + "        min-width: 414px !important;\r\n" + "    }\r\n" + "}\r\n"
			+ "\r\n" + "    </style>\r\n" + "\r\n" + "    <!-- CSS Reset : END -->\r\n" + "\r\n"
			+ "    <!-- Progressive Enhancements : BEGIN -->\r\n" + "    <style>\r\n" + "\r\n" + "	    .primary{\r\n"
			+ "	background: #30e3ca;\r\n" + "}\r\n" + ".bg_white{\r\n" + "	background: #ffffff;\r\n" + "}\r\n"
			+ ".bg_light{\r\n" + "	background: #fafafa;\r\n" + "}\r\n" + ".bg_black{\r\n"
			+ "	background: #000000;\r\n" + "}\r\n" + ".bg_dark{\r\n" + "	background: rgba(0,0,0,.8);\r\n" + "}\r\n"
			+ ".email-section{\r\n" + "	padding:2.5em;\r\n" + "}\r\n" + "\r\n" + "/*BUTTON*/\r\n" + ".btn{\r\n"
			+ "	padding: 10px 15px;\r\n" + "	display: inline-block;\r\n" + "}\r\n" + ".btn.btn-primary{\r\n"
			+ "	border-radius: 5px;\r\n" + "	background: #30e3ca;\r\n" + "	color: #ffffff;\r\n" + "}\r\n"
			+ ".btn.btn-white{\r\n" + "	border-radius: 5px;\r\n" + "	background: #ffffff;\r\n"
			+ "	color: #000000;\r\n" + "}\r\n" + ".btn.btn-white-outline{\r\n" + "	border-radius: 5px;\r\n"
			+ "	background: transparent;\r\n" + "	border: 1px solid #fff;\r\n" + "	color: #fff;\r\n" + "}\r\n"
			+ ".btn.btn-black-outline{\r\n" + "	border-radius: 0px;\r\n" + "	background: transparent;\r\n"
			+ "	border: 2px solid #000;\r\n" + "	color: #000;\r\n" + "	font-weight: 700;\r\n" + "}\r\n" + "\r\n"
			+ "h1,h2,h3,h4,h5,h6{\r\n" + "	font-family: 'Times New Roman', Times, serif;\r\n" + "	color: #000000;\r\n"
			+ "	margin-top: 0;\r\n" + "	font-weight: 400;\r\n" + "}\r\n" + "\r\n" + "body{\r\n"
			+ "	font-family: 'Lato', sans-serif;\r\n" + "	font-weight: 400;\r\n" + "	font-size: 15px;\r\n"
			+ "	line-height: 1.8;\r\n" + "	color: rgba(0,0,0,.4);\r\n" + "}\r\n" + "\r\n" + "a{\r\n"
			+ "	color: #30e3ca;\r\n" + "}\r\n" + "\r\n" + "table{\r\n" + "}\r\n" + "/*LOGO*/\r\n" + "\r\n"
			+ ".logo h1{\r\n" + "	margin: 0;\r\n" + "}\r\n" + ".logo h1 a{\r\n" + "	color: #30e3ca;\r\n"
			+ "	font-size: 24px;\r\n" + "	font-weight: 700;\r\n" + "	font-family: 'Lato', sans-serif;\r\n" + "}\r\n"
			+ "\r\n" + "/*HERO*/\r\n" + ".hero{\r\n" + "	position: relative;\r\n" + "	z-index: 0;\r\n" + "}\r\n"
			+ "\r\n" + ".hero .text{\r\n" + "	color: rgba(0,0,0,.3);\r\n" + "}\r\n" + ".hero .text h2{\r\n"
			+ "	color: #000;\r\n" + "	font-size: 40px;\r\n" + "	margin-bottom: 0;\r\n" + "	font-weight: 400;\r\n"
			+ "	line-height: 1.4;\r\n" + "}\r\n" + ".hero .text h3{\r\n" + "	font-size: 24px;\r\n"
			+ "	font-weight: 300;\r\n" + "}\r\n" + ".hero .text h2 span{\r\n" + "	font-weight: 600;\r\n"
			+ "	color: #30e3ca;\r\n" + "}\r\n" + "\r\n" + "\r\n" + "/*HEADING SECTION*/\r\n" + ".heading-section{\r\n"
			+ "}\r\n" + ".heading-section h2{\r\n" + "	color: #000000;\r\n" + "	font-size: 28px;\r\n"
			+ "	margin-top: 0;\r\n" + "	line-height: 1.4;\r\n" + "	font-weight: 400;\r\n" + "}\r\n"
			+ ".heading-section .subheading{\r\n" + "	margin-bottom: 20px !important;\r\n"
			+ "	display: inline-block;\r\n" + "	font-size: 13px;\r\n" + "	text-transform: uppercase;\r\n"
			+ "	letter-spacing: 2px;\r\n" + "	color: rgba(0,0,0,.4);\r\n" + "	position: relative;\r\n" + "}\r\n"
			+ ".heading-section .subheading::after{\r\n" + "	position: absolute;\r\n" + "	left: 0;\r\n"
			+ "	right: 0;\r\n" + "	bottom: -10px;\r\n" + "	content: '';\r\n" + "	width: 100%;\r\n"
			+ "	height: 2px;\r\n" + "	background: #30e3ca;\r\n" + "	margin: 0 auto;\r\n" + "}\r\n" + "\r\n"
			+ ".heading-section-white{\r\n" + "	color: rgba(255,255,255,.8);\r\n" + "}\r\n"
			+ ".heading-section-white h2{\r\n" + "	/* font-family:  */\r\n" + "	line-height: 1;\r\n"
			+ "	padding-bottom: 0;\r\n" + "}\r\n" + ".heading-section-white h2{\r\n" + "	color: #ffffff;\r\n"
			+ "}\r\n" + ".heading-section-white .subheading{\r\n" + "	margin-bottom: 0;\r\n"
			+ "	display: inline-block;\r\n" + "	font-size: 13px;\r\n" + "	text-transform: uppercase;\r\n"
			+ "	letter-spacing: 2px;\r\n" + "	color: rgba(255,255,255,.4);\r\n" + "}\r\n" + "\r\n" + "\r\n"
			+ "ul.social{\r\n" + "	padding: 0;\r\n" + "}\r\n" + "ul.social li{\r\n" + "	display: inline-block;\r\n"
			+ "	margin-right: 10px;\r\n" + "}\r\n" + "\r\n" + "/*FOOTER*/\r\n" + "\r\n" + ".footer{\r\n"
			+ "	border-top: 1px solid rgba(0,0,0,.05);\r\n" + "	color: rgba(0,0,0,.5);\r\n" + "}\r\n"
			+ ".footer .heading{\r\n" + "	color: #000;\r\n" + "	font-size: 20px;\r\n" + "}\r\n" + ".footer ul{\r\n"
			+ "	margin: 0;\r\n" + "	padding: 0;\r\n" + "}\r\n" + ".footer ul li{\r\n" + "	list-style: none;\r\n"
			+ "	margin-bottom: 10px;\r\n" + "}\r\n" + ".footer ul li a{\r\n" + "	color: rgba(0,0,0,1);\r\n" + "}\r\n"
			+ "\r\n" + "\r\n" + "@media screen and (max-width: 500px) {\r\n" + "\r\n" + "\r\n" + "}\r\n" + "\r\n"
			+ "\r\n" + "    </style>\r\n" + "\r\n" + "\r\n" + "</head>\r\n" + "\r\n"
			+ "<body width=\"100%\" style=\"margin: 0; padding: 0 !important; mso-line-height-rule: exactly; background-color: #f1f1f1;\">\r\n"
			+ "	<center style=\"width: 100%; background-color: #f1f1f1;\">\r\n"
			+ "    <div style=\"display: none; font-size: 1px;max-height: 0px; max-width: 0px; opacity: 0; overflow: hidden; mso-hide: all; font-family: sans-serif;\">\r\n"
			+ "      &zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;\r\n"
			+ "    </div>\r\n" + "    <div style=\"max-width: 600px; margin: 0 auto;\" class=\"email-container\">\r\n"
			+ "    	<!-- BEGIN BODY -->\r\n"
			+ "      <table align=\"center\" role=\"presentation\" cellspacing=\"0\" cellpadding=\"0\" border=\"0\" width=\"100%\" style=\"margin: auto;\">\r\n"
			+ "      	<tr>\r\n"
			+ "          <td valign=\"top\" class=\"bg_white\" style=\"padding: 1em 2.5em 0 2.5em;\">\r\n"
			+ "          	<table role=\"presentation\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" width=\"100%\">\r\n"
			+ "          		<tr>\r\n" + "          			<td class=\"logo\" style=\"text-align: center;\">\r\n"
			+ "			            <h1><a href=\"#\">Email</a></h1>\r\n" + "			          </td>\r\n"
			+ "          		</tr>\r\n" + "          	</table>\r\n" + "          </td>\r\n"
			+ "	      </tr><!-- end tr -->\r\n" + "	      <tr>\r\n"
			+ "          <td valign=\"middle\" class=\"hero bg_white\" style=\"padding: 3em 0 2em 0;\">\r\n"
			+ "            <img src=\"https://firebasestorage.googleapis.com/v0/b/davitickets-2e627.appspot.com/o/email.png?alt=media&token=6f31d4c1-1a7d-4fb5-bf90-f836224f0ea6\" alt=\"\" style=\"width: 300px; max-width: 600px; height: auto; margin: auto; display: block;\">\r\n"
			+ "          </td>\r\n" + "	      </tr><!-- end tr -->\r\n" + "				<tr>\r\n"
			+ "          <td valign=\"middle\" class=\"hero bg_white\" style=\"padding: 2em 0 4em 0;\">\r\n"
			+ "            <table>\r\n" + "            	<tr>\r\n" + "            		<td>\r\n";

	String footer = "</td>\r\n" + "            	</tr>\r\n" + "            </table>\r\n" + "          </td>\r\n"
			+ "	      </tr><!-- end tr -->\r\n" + "      <!-- 1 Column Text + Button : END -->\r\n"
			+ "      </table>\r\n"
			+ "      <table align=\"center\" role=\"presentation\" cellspacing=\"0\" cellpadding=\"0\" border=\"0\" width=\"100%\" style=\"margin: auto;\">\r\n"
			+ "      	<tr>\r\n" + "          <td valign=\"middle\" class=\"bg_light footer email-section\">\r\n"
			+ "            <table>\r\n" + "            	<tr>\r\n"
			+ "                <td valign=\"top\" width=\"33.333%\" style=\"padding-top: 20px;\">\r\n"
			+ "                  <table role=\"presentation\" cellspacing=\"0\" cellpadding=\"0\" border=\"0\" width=\"100%\">\r\n"
			+ "                    <tr>\r\n"
			+ "                      <td style=\"text-align: left; padding-right: 10px;\">\r\n"
			+ "                      	<h3 class=\"heading\">Về chúng tôi</h3>\r\n"
			+ "                      	<p>Website tư vấn sinh viên hcmute\r\n" + "                            </p>\r\n"
			+ "                      </td>\r\n" + "                    </tr>\r\n" + "                  </table>\r\n"
			+ "                </td>\r\n"
			+ "                <td valign=\"top\" width=\"33.333%\" style=\"padding-top: 20px;\">\r\n"
			+ "                  <table role=\"presentation\" cellspacing=\"0\" cellpadding=\"0\" border=\"0\" width=\"100%\">\r\n"
			+ "                    <tr>\r\n"
			+ "                      <td style=\"text-align: left; padding-left: 5px; padding-right: 5px;\">\r\n"
			+ "                      	<h3 class=\"heading\">Liên hệ</h3>\r\n" + "                      	<ul>\r\n"
			+ "					                <li><span class=\"text\">HCMUTE</span></li>\r\n"
			+ "					                <li><span class=\"text\">0974117373</span></a></li>\r\n"
			+ "					              </ul>\r\n" + "                      </td>\r\n"
			+ "                    </tr>\r\n" + "                  </table>\r\n" + "                </td>\r\n"
			+ "                <!-- <td valign=\"top\" width=\"33.333%\" style=\"padding-top: 20px;\">\r\n"
			+ "                  <table role=\"presentation\" cellspacing=\"0\" cellpadding=\"0\" border=\"0\" width=\"100%\">\r\n"
			+ "                    <tr>\r\n"
			+ "                      <td style=\"text-align: left; padding-left: 10px;\">\r\n"
			+ "                      	<h3 class=\"heading\">Useful Links</h3>\r\n"
			+ "                      	<ul>\r\n"
			+ "					                <li><a href=\"#\">Home</a></li>\r\n"
			+ "					                <li><a href=\"#\">About</a></li>\r\n"
			+ "					                <li><a href=\"#\">Services</a></li>\r\n"
			+ "					                <li><a href=\"#\">Work</a></li>\r\n"
			+ "					              </ul>\r\n" + "                      </td>\r\n"
			+ "                    </tr>\r\n" + "                  </table>\r\n" + "                </td>\r\n"
			+ "              </tr>\r\n" + "            </table>\r\n" + "          </td> -->\r\n"
			+ "        </tr><!-- end: tr -->\r\n" + "        <!-- <tr>\r\n"
			+ "          <td class=\"bg_light\" style=\"text-align: center;\">\r\n"
			+ "          	<p>No longer want to receive these email? You can <a href=\"#\" style=\"color: rgba(0,0,0,.8);\">Unsubscribe here</a></p>\r\n"
			+ "          </td>\r\n" + "        </tr> -->\r\n" + "      </table>\r\n" + "\r\n" + "    </div>\r\n"
			+ "  </center>\r\n" + "</body>\r\n" + "</html>";
}
