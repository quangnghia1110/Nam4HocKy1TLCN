package studentConsulting.controller.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.*;
import studentConsulting.model.payload.request.ChangePasswordRequest;
import studentConsulting.model.payload.request.UpdateInformationRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.implement.common.FileStorageServiceImpl;
import studentConsulting.service.implement.common.UserServiceImpl;

import javax.validation.Valid;
import java.security.Principal;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class UserController {

    @Autowired
    private UserServiceImpl userService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private FileStorageServiceImpl fileStorageService;

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
                                                              @RequestParam(value = "username", required = false) String username,
                                                              @RequestParam(value = "studentCode", required = false) String studentCode,
                                                              @RequestParam(value = "schoolName", required = false) String schoolName,
                                                              @RequestParam(value = "firstName", required = false) String firstName,
                                                              @RequestParam(value = "lastName", required = false) String lastName,
                                                              @RequestParam(value = "phone", required = false) String phone,
                                                              @RequestParam(value = "gender", required = false) String gender,
                                                              @RequestParam(value = "email", required = false) String email,
                                                              @RequestParam(value = "avatarUrl", required = false) String avatarUrl,
                                                              @RequestParam(value = "addressLine", required = false) String addressLine,
                                                              @RequestParam(value = "provinceCode", required = false) String provinceCode,
                                                              @RequestParam(value = "districtCode", required = false) String districtCode,
                                                              @RequestParam(value = "wardCode", required = false) String wardCode,
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

    @GetMapping("/address/provinces")
    public ResponseEntity<DataResponse<List<ProvinceDTO>>> getAllProvinces() {
        List<ProvinceDTO> provinces = userService.getAllProvinces();
        DataResponse<List<ProvinceDTO>> response = DataResponse.<List<ProvinceDTO>>builder()
                .status("success")
                .message("Lấy thành công danh sách các tỉnh/thành phố.")
                .data(provinces)
                .build();

        return ResponseEntity.ok(response);
    }

    @GetMapping("/address/districts")
    public ResponseEntity<DataResponse<List<DistrictDTO>>> getDistrictsByProvince(@RequestParam String provinceCode) {
        List<DistrictDTO> districts = userService.getDistrictsByProvince(provinceCode);
        DataResponse<List<DistrictDTO>> response = DataResponse.<List<DistrictDTO>>builder()
                .status("success")
                .message("Lấy thành công danh sách quận/huyện cho mã tỉnh: " + provinceCode)
                .data(districts)
                .build();

        return ResponseEntity.ok(response);
    }

    @GetMapping("/address/wards")
    public ResponseEntity<DataResponse<List<WardDTO>>> getWardsByDistrict(@RequestParam String districtCode) {
        List<WardDTO> wards = userService.getWardsByDistrict(districtCode);
        DataResponse<List<WardDTO>> response = DataResponse.<List<WardDTO>>builder()
                .status("success")
                .message("Lấy thành công danh sách phường/xã cho mã quận/huyện: " + districtCode)
                .data(wards)
                .build();

        return ResponseEntity.ok(response);
    }


}
