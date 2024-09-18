package studentConsulting.service;

import java.util.List;
import java.util.Optional;

import studentConsulting.model.entity.authentication.UserInformationEntity;
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

public interface IUserService {
    public DataResponse<DataResponse.LoginData> refreshToken(String refreshToken);

    public DataResponse<UserInformationDTO> register(RegisterRequest registerRequest);

    public DataResponse<Object> confirmRegistration(ConfirmRegistrationRequest confirmRegistrationRequest);

    public DataResponse<DataResponse.LoginData> login(LoginRequest loginRequest);

    public DataResponse<Object> changePassword(String username, ChangePasswordRequest changePasswordRequest);

    public DataResponse<Object> forgotPassword(ForgotPasswordRequest forgotPasswordRequest);

    public DataResponse<Object> checkVerifyCode(VerifyCodeCheckRequest verifyCode);

    public DataResponse<Object> resetPassword(ResetPasswordRequest resetPasswordRequest);

    public Iterable<UserInformationEntity> getAllUser();
    
    public DataResponse<Object> resendVerificationCodeForRegister(ResendVerificationRequest resendRequest);
    
    public DataResponse<Object> resendVerificationCodeForForgotPassword(ResendVerificationRequest resendRequest);
    
    public DataResponse<Object> changeEmail(ChangeEmailRequest changeEmailRequest);

    public UserInformationDTO getProfile(Integer idUser);
    
    public DataResponse<Object> updateProfile(Integer userId, UpdateInformationRequest userUpdateRequest);
    
    public Integer getUserIdByUsername(String username);

    public Optional<UserInformationEntity> findByFullName(String fullName);
    
    List<UserInformationEntity> findConsultantsByDepartmentId(Integer departmentId);

    public Optional<UserInformationEntity> findConsultantById(Integer consultantId);
    public Integer getUserIdByEmail(String email);


}
