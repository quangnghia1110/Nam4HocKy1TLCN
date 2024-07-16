package studentConsulting.service;

import studentConsulting.entity.authentication.UserEntity;
import studentConsulting.request.authentication.ChangePasswordRequest;
import studentConsulting.request.authentication.ConfirmRegistrationRequest;
import studentConsulting.request.authentication.ForgotPasswordRequest;
import studentConsulting.request.authentication.LoginRequest;
import studentConsulting.request.authentication.RegisterRequest;
import studentConsulting.request.authentication.ResetPasswordRequest;
import studentConsulting.request.authentication.UpdateInformationRequest;
import studentConsulting.request.authentication.VerifyCodeCheckRequest;
import studentConsulting.response.DataResponse;
import studentConsulting.response.LoginResponse;
import studentConsulting.response.RegisterResponse;

public interface IUserService {
    public LoginResponse refreshToken(String refreshToken);
    public LoginResponse login(LoginRequest loginRequest);
    public RegisterResponse register(RegisterRequest registerRequest);
    public DataResponse<Object> changePassword(String username, ChangePasswordRequest changePasswordRequest);
    public DataResponse<Object> forgotPassword(ForgotPasswordRequest forgotPasswordRequest);
    public DataResponse<Object> checkVerifyCode(VerifyCodeCheckRequest verifyCode);
    public DataResponse<Object> resetPassword(ResetPasswordRequest resetPasswordRequest);
    public Iterable<UserEntity> getAllUser();
    public DataResponse<Object> getProfile(Long idUser);
    public DataResponse<Object> updateProfile(Long idUser, UpdateInformationRequest userUpdateRequest);
    public DataResponse<Object> deleteUser(Long idUser);
    public DataResponse<Object> unlockUser(Long idUser);
    public DataResponse<Object> confirmRegistration(ConfirmRegistrationRequest confirmRegistrationRequest); 

}
