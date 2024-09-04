package studentConsulting.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import studentConsulting.model.payload.request.authentication.*;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.implement.UserServiceImpl;
import studentConsulting.model.payload.dto.UserInformationDTO;

import javax.validation.Valid;

@RestController
@RequestMapping(value = "/api/v1/auth")
public class AuthController {

    @Autowired
    private UserServiceImpl userService;

    @GetMapping(value = "/refresh/{refresh-token}")
    public ResponseEntity<DataResponse<DataResponse.LoginData>> refreshToken(@PathVariable("refresh-token") String refreshToken) {
        return ResponseEntity.ok(userService.refreshToken(refreshToken));
    }
    
    @PostMapping(value = "/register")
    public ResponseEntity<DataResponse<UserInformationDTO>> registerUser(@Valid @RequestBody RegisterRequest userRegisterRequest) {
        return ResponseEntity.ok(userService.register(userRegisterRequest));
    }

    @PostMapping(value = "/confirm-registration")
    public ResponseEntity<DataResponse<Object>> confirmRegistration(@Valid @RequestBody ConfirmRegistrationRequest confirmRegistrationRequest) {
        return ResponseEntity.ok(userService.confirmRegistration(confirmRegistrationRequest));
    }
    
    @PostMapping(value = "/login")
    public ResponseEntity<DataResponse<DataResponse.LoginData>> login(@Valid @RequestBody LoginRequest loginRequest) {
        return ResponseEntity.ok(userService.login(loginRequest));
    }

    @PostMapping(value = "/forgot-password")
    public ResponseEntity<DataResponse<Object>> forgotPassword(@Valid @RequestBody ForgotPasswordRequest forgotPasswordRequest) {
        return ResponseEntity.ok(userService.forgotPassword(forgotPasswordRequest));
    }

    @PostMapping(value = "/verify-code")
    public ResponseEntity<DataResponse<Object>> checkVerifyCode(@Valid @RequestBody VerifyCodeCheckRequest verifyCodeCheckRequest) {
        return ResponseEntity.ok(userService.checkVerifyCode(verifyCodeCheckRequest));
    }

    @PostMapping(value = "/reset-password")
    public ResponseEntity<DataResponse<Object>> resetPassword(@Valid @RequestBody ResetPasswordRequest resetPasswordRequest) {
        return ResponseEntity.ok(userService.resetPassword(resetPasswordRequest));
    }
    
    @PostMapping(value = "/resend-register-verification-code")
    public ResponseEntity<DataResponse<Object>> resendRegisterVerificationCode(@Valid @RequestBody ResendVerificationRequest resendVerificationRequest) {
        return ResponseEntity.ok(userService.resendVerificationCodeForRegister(resendVerificationRequest));
    }

    @PostMapping(value = "/resend-forgot-password-verification-code")
    public ResponseEntity<DataResponse<Object>> resendForgotPasswordVerificationCode(@Valid @RequestBody ResendVerificationRequest resendVerificationRequest) {
        return ResponseEntity.ok(userService.resendVerificationCodeForForgotPassword(resendVerificationRequest));
    }

    
    @PostMapping(value = "/change-email")
    public ResponseEntity<DataResponse<Object>> changeEmail(@Valid @RequestBody ChangeEmailRequest changeEmailRequest) {
        return ResponseEntity.ok(userService.changeEmail(changeEmailRequest));
    }
}
