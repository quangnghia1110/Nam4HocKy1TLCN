package studentConsulting.service.interfaces.common;

import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.UserInformationDTO;
import studentConsulting.model.payload.request.*;
import studentConsulting.model.payload.response.DataResponse;

import java.util.List;
import java.util.Optional;

public interface IUserService {
    DataResponse<DataResponse.LoginData> refreshToken(String refreshToken);

    DataResponse<UserInformationDTO> register(RegisterRequest registerRequest);

    DataResponse<Object> confirmRegistration(ConfirmRegistrationRequest confirmRegistrationRequest);

    DataResponse<DataResponse.LoginData> login(LoginRequest loginRequest);

    DataResponse<Object> changePassword(String username, ChangePasswordRequest changePasswordRequest);

    DataResponse<Object> forgotPassword(ForgotPasswordRequest forgotPasswordRequest);

    DataResponse<Object> checkVerifyCode(VerifyCodeCheckRequest verifyCode);

    DataResponse<Object> resetPassword(ResetPasswordRequest resetPasswordRequest);

    DataResponse<Object> resendVerificationCodeForRegister(ResendVerificationRequest resendRequest);

    DataResponse<Object> resendVerificationCodeForForgotPassword(ResendVerificationRequest resendRequest);

    DataResponse<Object> changeEmail(ChangeEmailRequest changeEmailRequest);

    DataResponse<Object> updateProfile(Integer userId, UpdateInformationRequest userUpdateRequest);

    List<UserInformationEntity> findConsultantsByDepartmentId(Integer departmentId);

    Optional<UserInformationEntity> findConsultantById(Integer consultantId);

    Integer getUserIdByEmail(String email);

    Optional<UserInformationEntity> findById(Integer id);


}
