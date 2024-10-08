package studentConsulting.controller.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.address.AddressDTO;
import studentConsulting.model.payload.dto.authentication.AccountDTO;
import studentConsulting.model.payload.dto.user.UserInformationDTO;
import studentConsulting.model.payload.request.authentication.ChangePasswordRequest;
import studentConsulting.model.payload.request.authentication.UpdateInformationRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.implement.common.CommonFileStorageServiceImpl;
import studentConsulting.service.implement.common.CommonUserServiceImpl;

import javax.validation.Valid;
import java.security.Principal;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class CommonUserController {

    @Autowired
    private CommonUserServiceImpl userService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private CommonFileStorageServiceImpl fileStorageService;

    @PutMapping(value = "/profile/change-password")
    public ResponseEntity<DataResponse<Object>> changePassword(Principal principal,
                                                               @Valid @RequestBody ChangePasswordRequest changePasswordRequest) {
        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        DataResponse<Object> response = userService.changePassword(user.getAccount().getEmail(), changePasswordRequest);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @GetMapping("/profile")
    public ResponseEntity<DataResponse<UserInformationDTO>> getProfile(Principal principal) {
        try {
            String email = principal.getName();
            System.out.println("Email: " + email);
            Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
            if (!userOpt.isPresent()) {
                throw new ErrorException("Không tìm thấy người dùng");
            }

            UserInformationEntity userEntity = userOpt.get();

            AddressDTO addressDto = null;
            if (userEntity.getAddress() != null) {
                addressDto = AddressDTO.builder().line(userEntity.getAddress().getLine())
                        .provinceCode(userEntity.getAddress().getProvince().getCode())
                        .districtCode(userEntity.getAddress().getDistrict().getCode())
                        .wardCode(userEntity.getAddress().getWard().getCode()).build();
            }

            UserInformationDTO userDto = UserInformationDTO.builder().username(userEntity.getAccount().getUsername())
                    .schoolName(userEntity.getSchoolName()).firstName(userEntity.getFirstName())
                    .lastName(userEntity.getLastName()).phone(userEntity.getPhone())
                    .avatarUrl(userEntity.getAvatarUrl()).gender(userEntity.getGender())
                    .email(userEntity.getAccount().getEmail()).address(addressDto) // Sử dụng addressDto
                    .account(AccountDTO.builder().email(userEntity.getAccount().getEmail())
                            .username(userEntity.getAccount().getUsername()).build())
                    .build();

            return ResponseEntity.ok(DataResponse.<UserInformationDTO>builder().status("success")
                    .message("Thông tin người dùng").data(userDto).build());
        } catch (Exception e) {
            throw new ErrorException("JWT đã hết hạn. Vui lòng đăng nhập lại.");
        }
    }

    @PutMapping(value = "/profile/update", consumes = {MediaType.MULTIPART_FORM_DATA_VALUE})
    public ResponseEntity<DataResponse<Object>> updateProfile(Principal principal,
                                                              @RequestParam("username") String username,
                                                              @RequestParam("studentCode") String studentCode,
                                                              @RequestParam("schoolName") String schoolName,
                                                              @RequestParam("firstName") String firstName,
                                                              @RequestParam("lastName") String lastName,
                                                              @RequestParam("phone") String phone,
                                                              @RequestParam("gender") String gender,
                                                              @RequestParam("email") String email,
                                                              @RequestParam(value = "avatarUrl", required = false) String avatarUrl,
                                                              @RequestParam("addressLine") String addressLine,
                                                              @RequestParam("provinceCode") String provinceCode,
                                                              @RequestParam("districtCode") String districtCode,
                                                              @RequestParam("wardCode") String wardCode,
                                                              @RequestPart(value = "file", required = false) MultipartFile file) {

        String userEmail = principal.getName();
        System.out.println("Email: " + userEmail);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(userEmail);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity userEntity = userOpt.get();
        Integer userId = userService.getUserIdByEmail(userEmail);

        if (file != null && !file.isEmpty()) {
            avatarUrl = fileStorageService.saveFile(file);
        } else {
            avatarUrl = userEntity.getAvatarUrl();
        }

        UpdateInformationRequest userUpdateRequest = UpdateInformationRequest.builder()
                .username(username != null ? username : userEntity.getAccount().getUsername())
                .studentCode(studentCode != null ? studentCode : userEntity.getStudentCode())
                .schoolName(schoolName != null ? schoolName : userEntity.getSchoolName())
                .firstName(firstName != null ? firstName : userEntity.getFirstName())
                .lastName(lastName != null ? lastName : userEntity.getLastName())
                .phone(phone != null ? phone : userEntity.getPhone())
                .avatarUrl(avatarUrl)
                .gender(gender != null ? gender : userEntity.getGender())
                .email(email != null ? email : userEntity.getAccount().getEmail())
                .address(AddressDTO.builder()
                        .line(addressLine != null ? addressLine : userEntity.getAddress().getLine())
                        .provinceCode(provinceCode != null ? provinceCode : userEntity.getAddress().getProvince().getCode())
                        .districtCode(districtCode != null ? districtCode : userEntity.getAddress().getDistrict().getCode())
                        .wardCode(wardCode != null ? wardCode : userEntity.getAddress().getWard().getCode())
                        .build())
                .build();

        DataResponse<Object> response = userService.updateProfile(userId, userUpdateRequest);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }


}
