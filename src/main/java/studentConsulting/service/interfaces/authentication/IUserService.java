package studentConsulting.service.interfaces.authentication;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.UserInformationDTO;
import studentConsulting.model.payload.request.authentication.*;
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

    public UserInformationDTO getProfile(Integer idUser);
}
