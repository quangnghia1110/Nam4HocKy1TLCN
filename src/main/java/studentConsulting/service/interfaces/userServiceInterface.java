package studentConsulting.service.interfaces;

import studentConsulting.entity.authentication.userEntity;
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

public interface userServiceInterface {
    public loginResponse refreshToken(String refreshToken);
    public loginResponse login(loginRequest loginRequest);
    public registerResponse register(registerRequest registerRequest);
    public apiResponse<Object> changePassword(String username, changePasswordRequest changePasswordRequest);
    public apiResponse<Object> forgotPassword(forgotPasswordRequest forgotPasswordRequest);
    public apiResponse<Object> checkVerifyCode(verifyCodeCheckRequest verifyCode);
    public apiResponse<Object> resetPassword(resetPasswordRequest resetPasswordRequest);
    public Iterable<userEntity> getAllUser();
    public apiResponse<Object> getProfile(Long idUser);
    public apiResponse<Object> updateProfile(Long idUser, updateInformationRequest userUpdateRequest);
    public apiResponse<Object> deleteUser(Long idUser);
    public apiResponse<Object> unlockUser(Long idUser);
    public apiResponse<Object> confirmRegistration(confirmRegistrationRequest confirmRegistrationRequest); 

}
