package studentConsulting.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import studentConsulting.request.authentication.*;
import studentConsulting.response.apiResponse;
import studentConsulting.response.loginResponse;
import studentConsulting.response.registerResponse;
import studentConsulting.security.UserPrinciple.*;
import studentConsulting.service.implement.userServiceImpl;

import javax.validation.Valid;
import java.security.Principal;

@RestController
@RequestMapping(value = "/api/auth")
public class AuthController {

    @Autowired
    private userServiceImpl userService;

    @PostMapping(value = "/register")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Successful registration", content = @Content(schema = @Schema(implementation = registerResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public registerResponse registerUser(@Valid @RequestBody registerRequest userRegisterRequest) {
        return userService.register(userRegisterRequest);
    }

    @PostMapping(value = "/login")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Successful login", content = @Content(schema = @Schema(implementation = loginResponse.class))),
        @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public loginResponse login(@Valid @RequestBody loginRequest loginRequest) {
        return userService.login(loginRequest);
    }

    @GetMapping(value = "/refresh/{refresh-token}")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Successful token refresh", content = @Content(schema = @Schema(implementation = loginResponse.class))),
        @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public loginResponse refreshToken(@PathVariable("refresh-token") String refreshToken) {
        return userService.refreshToken(refreshToken);
    }

    @PutMapping(value = "/change-password")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Password changed successfully", content = @Content(schema = @Schema(implementation = apiResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public apiResponse<Object> changePassword(Principal principal, @Valid @RequestBody changePasswordRequest changePasswordRequest) {
        return userService.changePassword(principal.getName(), changePasswordRequest);
    }

    @PostMapping(value = "/forgot-password")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Request processed successfully", content = @Content(schema = @Schema(implementation = apiResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public apiResponse<Object> forgotPassword(@Valid @RequestBody forgotPasswordRequest forgotPasswordRequest) {
        return userService.forgotPassword(forgotPasswordRequest);
    }

    @PostMapping(value = "/verify-code")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Verification code checked successfully", content = @Content(schema = @Schema(implementation = apiResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public apiResponse<Object> checkVerifyCode(@Valid @RequestBody verifyCodeCheckRequest verifyCodeCheckRequest) {
        return userService.checkVerifyCode(verifyCodeCheckRequest);
    }

    @PostMapping(value = "/reset-password")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Password reset successfully", content = @Content(schema = @Schema(implementation = apiResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public apiResponse<Object> resetPassword(@Valid @RequestBody resetPasswordRequest resetPasswordRequest) {
        return userService.resetPassword(resetPasswordRequest);
    }

    @GetMapping(value = "/profile")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Profile fetched successfully", content = @Content(schema = @Schema(implementation = apiResponse.class))),
        @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public apiResponse<Object> getProfile(UserPrincipal userPrincipal) {
        return userService.getProfile(userPrincipal.getUserId());
    }

    @PutMapping(value = "/profile/update")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Profile updated successfully", content = @Content(schema = @Schema(implementation = apiResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public apiResponse<Object> updateProfile(UserPrincipal userPrincipal, @Valid @RequestBody updateInformationRequest userUpdateRequest) {
        return userService.updateProfile(userPrincipal.getUserId(), userUpdateRequest);
    }

    @PostMapping(value = "/confirm-registration")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Registration confirmed successfully", content = @Content(schema = @Schema(implementation = apiResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public apiResponse<Object> confirmRegistration(@Valid @RequestBody confirmRegistrationRequest confirmRegistrationRequest) {
        return userService.confirmRegistration(confirmRegistrationRequest);
    }
}
