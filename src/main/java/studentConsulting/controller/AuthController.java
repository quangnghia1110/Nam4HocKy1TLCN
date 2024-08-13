package studentConsulting.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import studentConsulting.model.payload.request.authentication.*;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.model.payload.response.LoginResponse;
import studentConsulting.model.payload.response.RegisterResponse;
import studentConsulting.security.userPrinciple.UserPrincipal;
import studentConsulting.service.implement.authentication.UserServiceImpl;

import javax.validation.Valid;
import java.security.Principal;

@RestController
@RequestMapping(value = "/api/v1/auth")
public class AuthController {

    @Autowired
    private UserServiceImpl userService;

    @GetMapping(value = "/refresh/{refresh-token}")
    public ResponseEntity<LoginResponse> refreshToken(@PathVariable("refresh-token") String refreshToken) {
        LoginResponse response = userService.refreshToken(refreshToken);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }
    
    @PostMapping(value = "/register")
    public ResponseEntity<RegisterResponse> registerUser(@Valid @RequestBody RegisterRequest userRegisterRequest) {
        RegisterResponse response = userService.register(userRegisterRequest);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @PostMapping(value = "/confirm-registration")
    public ResponseEntity<DataResponse<Object>> confirmRegistration(@Valid @RequestBody ConfirmRegistrationRequest confirmRegistrationRequest) {
        DataResponse<Object> response = userService.confirmRegistration(confirmRegistrationRequest);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }
    
    @PostMapping(value = "/login")
    public ResponseEntity<LoginResponse> login(@Valid @RequestBody LoginRequest loginRequest) {
        LoginResponse response = userService.login(loginRequest);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @PostMapping(value = "/forgot-password")
    public ResponseEntity<DataResponse<Object>> forgotPassword(@Valid @RequestBody ForgotPasswordRequest forgotPasswordRequest) {
        DataResponse<Object> response = userService.forgotPassword(forgotPasswordRequest);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @PostMapping(value = "/verify-code")
    public ResponseEntity<DataResponse<Object>> checkVerifyCode(@Valid @RequestBody VerifyCodeCheckRequest verifyCodeCheckRequest) {
        DataResponse<Object> response = userService.checkVerifyCode(verifyCodeCheckRequest);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @PostMapping(value = "/reset-password")
    public ResponseEntity<DataResponse<Object>> resetPassword(@Valid @RequestBody ResetPasswordRequest resetPasswordRequest) {
        DataResponse<Object> response = userService.resetPassword(resetPasswordRequest);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }
}
