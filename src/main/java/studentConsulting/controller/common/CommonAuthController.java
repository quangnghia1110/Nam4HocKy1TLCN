package studentConsulting.controller.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import studentConsulting.model.entity.authentication.AccountEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.payload.dto.user.UserInformationDTO;
import studentConsulting.model.payload.dto.user.UserOnlineDTO;
import studentConsulting.model.payload.request.authentication.*;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.authentication.AccountRepository;
import studentConsulting.service.implement.common.CommonUserServiceImpl;
import studentConsulting.service.implement.common.StatusOnlineService;

import javax.validation.Valid;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class CommonAuthController {

    @Autowired
    private CommonUserServiceImpl userService;

    @Autowired
    private StatusOnlineService statusOnlineService;

    @Autowired
    private AccountRepository accountRepository;

    @PostMapping(value = "/auth/refresh")
    public ResponseEntity<DataResponse<DataResponse.LoginData>> refreshToken(@RequestBody RefreshTokenRequest refreshTokenRequest) {
        System.out.println("API /auth/refresh được gọi với refresh token");
        String refreshToken = refreshTokenRequest.getRefreshToken();
        return ResponseEntity.ok(userService.refreshToken(refreshToken));
    }


    @PostMapping(value = "/auth/register")
    public ResponseEntity<DataResponse<UserInformationDTO>> registerUser(@Valid @RequestBody RegisterRequest userRegisterRequest) {
        return ResponseEntity.ok(userService.register(userRegisterRequest));
    }

    @PostMapping(value = "/auth/confirm-registration")
    public ResponseEntity<DataResponse<Object>> confirmRegistration(@Valid @RequestBody ConfirmRegistrationRequest confirmRegistrationRequest) {
        return ResponseEntity.ok(userService.confirmRegistration(confirmRegistrationRequest));
    }

    @PostMapping(value = "/auth/login")
    public ResponseEntity<DataResponse<DataResponse.LoginData>> login(@Valid @RequestBody LoginRequest loginRequest) {
        DataResponse<DataResponse.LoginData> loginResponse = userService.login(loginRequest);

        return ResponseEntity.ok(loginResponse);
    }


    @PostMapping(value = "/auth/forgot-password")
    public ResponseEntity<DataResponse<Object>> forgotPassword(@Valid @RequestBody ForgotPasswordRequest forgotPasswordRequest) {
        return ResponseEntity.ok(userService.forgotPassword(forgotPasswordRequest));
    }

    @PostMapping(value = "/auth/verify-code")
    public ResponseEntity<DataResponse<Object>> checkVerifyCode(@Valid @RequestBody VerifyCodeCheckRequest verifyCodeCheckRequest) {
        return ResponseEntity.ok(userService.checkVerifyCode(verifyCodeCheckRequest));
    }

    @PostMapping(value = "/auth/reset-password")
    public ResponseEntity<DataResponse<Object>> resetPassword(@Valid @RequestBody ResetPasswordRequest resetPasswordRequest) {
        return ResponseEntity.ok(userService.resetPassword(resetPasswordRequest));
    }

    @PostMapping(value = "/auth/resend-register-verification-code")
    public ResponseEntity<DataResponse<Object>> resendRegisterVerificationCode(@Valid @RequestBody ResendVerificationRequest resendVerificationRequest) {
        return ResponseEntity.ok(userService.resendVerificationCodeForRegister(resendVerificationRequest));
    }

    @PostMapping(value = "/auth/resend-forgot-password-verification-code")
    public ResponseEntity<DataResponse<Object>> resendForgotPasswordVerificationCode(@Valid @RequestBody ResendVerificationRequest resendVerificationRequest) {
        return ResponseEntity.ok(userService.resendVerificationCodeForForgotPassword(resendVerificationRequest));
    }


    @PostMapping(value = "/auth/change-email")
    public ResponseEntity<DataResponse<Object>> changeEmail(@Valid @RequestBody ChangeEmailRequest changeEmailRequest) {
        return ResponseEntity.ok(userService.changeEmail(changeEmailRequest));
    }

    @GetMapping("/auth/online-users")
    public ResponseEntity<DataResponse<List<UserOnlineDTO>>> getOnlineUsers() {
        LocalDateTime now = LocalDateTime.now();
        List<UserOnlineDTO> onlineUsers = statusOnlineService.getOnlineUsers().entrySet().stream()
                .filter(entry -> ChronoUnit.SECONDS.between(entry.getValue(), now) < 300)
                .map(entry -> {
                    String email = entry.getKey();
                    AccountEntity account = accountRepository.findByEmail(email)
                            .orElseThrow(() -> new Exceptions.ErrorException("Người dùng không được tìm thấy với email"));

                    return new UserOnlineDTO(
                            account.getName(),
                            account.getEmail(),
                            account.getPhone(),
                            "Online"
                    );
                })
                .collect(Collectors.toList());

        return ResponseEntity.ok(new DataResponse<>("success", "Lấy danh sách người dùng trực tuyến thành công", onlineUsers));
    }

}
