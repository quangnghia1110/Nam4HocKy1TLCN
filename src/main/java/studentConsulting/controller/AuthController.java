package studentConsulting.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import studentConsulting.model.payload.request.authentication.ChangePasswordRequest;
import studentConsulting.model.payload.request.authentication.ConfirmRegistrationRequest;
import studentConsulting.model.payload.request.authentication.ForgotPasswordRequest;
import studentConsulting.model.payload.request.authentication.LoginRequest;
import studentConsulting.model.payload.request.authentication.RegisterRequest;
import studentConsulting.model.payload.request.authentication.ResetPasswordRequest;
import studentConsulting.model.payload.request.authentication.UpdateInformationRequest;
import studentConsulting.model.payload.request.authentication.VerifyCodeCheckRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.model.payload.response.LoginResponse;
import studentConsulting.model.payload.response.RegisterResponse;
import studentConsulting.security.userPrinciple.*;
import studentConsulting.service.implement.UserServiceImpl;

import javax.validation.Valid;
import java.security.Principal;

@RestController
@RequestMapping(value = "/api/v1/auth")
public class AuthController {

    @Autowired
    private UserServiceImpl userService;

    @PostMapping(value = "/register")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Successful registration", content = @Content(schema = @Schema(implementation = RegisterResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public RegisterResponse registerUser(@Valid @RequestBody RegisterRequest userRegisterRequest) {
        return userService.register(userRegisterRequest);
    }

    @PostMapping(value = "/login")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Successful login", content = @Content(schema = @Schema(implementation = LoginResponse.class))),
        @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public LoginResponse login(@Valid @RequestBody LoginRequest loginRequest) {
        return userService.login(loginRequest);
    }

    @GetMapping(value = "/refresh/{refresh-token}")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Successful token refresh", content = @Content(schema = @Schema(implementation = LoginResponse.class))),
        @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public LoginResponse refreshToken(@PathVariable("refresh-token") String refreshToken) {
        return userService.refreshToken(refreshToken);
    }

    @PutMapping(value = "/change-password")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Password changed successfully", content = @Content(schema = @Schema(implementation = DataResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public DataResponse<Object> changePassword(Principal principal, @Valid @RequestBody ChangePasswordRequest changePasswordRequest) {
        return userService.changePassword(principal.getName(), changePasswordRequest);
    }

    @PostMapping(value = "/forgot-password")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Request processed successfully", content = @Content(schema = @Schema(implementation = DataResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public DataResponse<Object> forgotPassword(@Valid @RequestBody ForgotPasswordRequest forgotPasswordRequest) {
        return userService.forgotPassword(forgotPasswordRequest);
    }

    @PostMapping(value = "/verify-code")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Verification code checked successfully", content = @Content(schema = @Schema(implementation = DataResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public DataResponse<Object> checkVerifyCode(@Valid @RequestBody VerifyCodeCheckRequest verifyCodeCheckRequest) {
        return userService.checkVerifyCode(verifyCodeCheckRequest);
    }

    @PostMapping(value = "/reset-password")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Password reset successfully", content = @Content(schema = @Schema(implementation = DataResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public DataResponse<Object> resetPassword(@Valid @RequestBody ResetPasswordRequest resetPasswordRequest) {
        return userService.resetPassword(resetPasswordRequest);
    }

    @GetMapping(value = "/profile")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Profile fetched successfully", content = @Content(schema = @Schema(implementation = DataResponse.class))),
        @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public DataResponse<Object> getProfile(UserPrincipal userPrincipal) {
        return userService.getProfile(userPrincipal.getUserId());
    }

    @PutMapping(value = "/profile/update")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Profile updated successfully", content = @Content(schema = @Schema(implementation = DataResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public DataResponse<Object> updateProfile(UserPrincipal userPrincipal, @Valid @RequestBody UpdateInformationRequest userUpdateRequest) {
        return userService.updateProfile(userPrincipal.getUserId(), userUpdateRequest);
    }

    @PostMapping(value = "/confirm-registration")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Registration confirmed successfully", content = @Content(schema = @Schema(implementation = DataResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public DataResponse<Object> confirmRegistration(@Valid @RequestBody ConfirmRegistrationRequest confirmRegistrationRequest) {
        return userService.confirmRegistration(confirmRegistrationRequest);
    }
}
