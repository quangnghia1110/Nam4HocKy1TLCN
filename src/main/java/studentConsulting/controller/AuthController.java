package studentConsulting.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

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
import studentConsulting.security.UserPrinciple.UserPrincipal;
import studentConsulting.service.implement.userServiceImpl;

import javax.validation.Valid;
import java.security.Principal;

@RestController
@RequestMapping(value = "/api/auth")
public class AuthController {

    @Autowired
    private userServiceImpl userService;

    @PostMapping(value = "/register")
    public registerResponse registerUser(@Valid @RequestBody registerRequest userRegisterRequest){
        registerResponse userRegisterResponse = userService.register(userRegisterRequest);
        return userRegisterResponse;
    }

    @PostMapping(value = "/login")
    public loginResponse login(@Valid @RequestBody loginRequest loginRequest)
    {
        loginResponse loginResponse = userService.login(loginRequest);
        return loginResponse;
    }

    @GetMapping(value = "/refresh/{refresh-token}")
    public loginResponse refreshToken(@PathVariable("refresh-token") String refreshToken)
    {
        return userService.refreshToken(refreshToken);
    }

    @PutMapping(value = "/change-password")
    public apiResponse<Object> changePass(Principal principal, @RequestBody changePasswordRequest changePasswordRequest)
    {
        return userService.changePassword(principal.getName(), changePasswordRequest);
    }

    @PostMapping(value = "/forgot-password")
    public apiResponse<Object> forgotPassword(@RequestBody forgotPasswordRequest forgotPasswordRequest)
    {
        return userService.forgotPassword(forgotPasswordRequest);
    }

    @PostMapping(value = "/verify-code")
    public apiResponse<Object> checkVerifyCode(@RequestBody verifyCodeCheckRequest verifyCodeCheckRequest)
    {
        return userService.checkVerifyCode(verifyCodeCheckRequest);
    }

    @PostMapping(value = "/reset-password")
    public apiResponse<Object> resetPassword(@RequestBody resetPasswordRequest resetPasswordRequest)
    {
        return userService.resetPassword(resetPasswordRequest);
    }

    @GetMapping(value = "/profile")
    public apiResponse<Object> getProfile(UserPrincipal userPrincipal)
    {
        return userService.getProfile(userPrincipal.getUserId());
    }

    @PutMapping(value = "/profile/update")
    public apiResponse<Object> updateProfile(UserPrincipal userPrincipal, @RequestBody updateInformationRequest userUpdateRequest)
    {
        return userService.updateProfile(userPrincipal.getUserId(), userUpdateRequest);
    }
    
    @PostMapping(value = "/confirm-registration")
    public apiResponse<Object> confirmRegistration(@RequestBody confirmRegistrationRequest confirmRegistrationRequest) {
        return userService.confirmRegistration(confirmRegistrationRequest);
    }
    
}
