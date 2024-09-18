package studentConsulting.controller;

import java.security.Principal;
import java.util.Optional;

import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.AccountDTO;
import studentConsulting.model.payload.dto.AddressDTO;
import studentConsulting.model.payload.dto.UserInformationDTO;
import studentConsulting.model.payload.request.authentication.ChangePasswordRequest;
import studentConsulting.model.payload.request.authentication.UpdateInformationRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.implement.UserServiceImpl;

@RestController
@RequestMapping("${base.url}")
public class UserController {

	@Autowired
	private UserServiceImpl userService;

	@Autowired
	private UserRepository userRepository;

	@PutMapping(value = "/profile/change-password")
	public ResponseEntity<DataResponse<Object>> changePassword(Principal principal,
			@Valid @RequestBody ChangePasswordRequest changePasswordRequest) {

		DataResponse<Object> response = userService.changePassword(principal.getName(), changePasswordRequest);
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

	@PutMapping(value = "/profile/update")
	public ResponseEntity<DataResponse<Object>> updateProfile(Principal principal,
			@Valid @RequestBody UpdateInformationRequest userUpdateRequest) {

		String email = principal.getName();
		System.out.println("Email: " + email);
		Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
		if (!userOpt.isPresent()) {
			throw new ErrorException("Không tìm thấy người dùng");
		}
		Integer userId = userService.getUserIdByEmail(email);

		DataResponse<Object> response = userService.updateProfile(userId, userUpdateRequest);
		return ResponseEntity.status(HttpStatus.OK).body(response);
	}

}
