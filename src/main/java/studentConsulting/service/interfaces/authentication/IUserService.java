package studentConsulting.service.interfaces.authentication;

import studentConsulting.model.entity.authentication.UserInformationEntity;
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

public interface IUserService {
    public LoginResponse refreshToken(String refreshToken);
    public RegisterResponse register(RegisterRequest registerRequest);
    public DataResponse<Object> confirmRegistration(ConfirmRegistrationRequest confirmRegistrationRequest); 
    public LoginResponse login(LoginRequest loginRequest);
    public DataResponse<Object> changePassword(String username, ChangePasswordRequest changePasswordRequest);
    public DataResponse<Object> forgotPassword(ForgotPasswordRequest forgotPasswordRequest);
    public DataResponse<Object> checkVerifyCode(VerifyCodeCheckRequest verifyCode);
    public DataResponse<Object> resetPassword(ResetPasswordRequest resetPasswordRequest);
    public Iterable<UserInformationEntity> getAllUser();
    public DataResponse<Object> getProfile(Long idUser);
    public DataResponse<Object> updateProfile(Long idUser, UpdateInformationRequest userUpdateRequest);
}
