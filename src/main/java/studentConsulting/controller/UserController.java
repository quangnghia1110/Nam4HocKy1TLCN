package studentConsulting.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import studentConsulting.model.exception.Exceptions.ResourceNotFoundException;
import studentConsulting.model.payload.dto.UserInformationDTO;
import studentConsulting.model.payload.request.authentication.*;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.security.userPrinciple.UserPrincipal;
import studentConsulting.service.implement.authentication.UserServiceImpl;

import javax.validation.Valid;
import java.security.Principal;

@RestController
@RequestMapping(value = "/api/v1/user")
public class UserController {

	@Autowired
	private UserServiceImpl userService;

	@PutMapping(value = "/change-password")
	public ResponseEntity<DataResponse<Object>> changePassword(Principal principal,
			@Valid @RequestBody ChangePasswordRequest changePasswordRequest) {
		DataResponse<Object> response = userService.changePassword(principal.getName(), changePasswordRequest);
		return ResponseEntity.status(HttpStatus.OK).body(response);
	}

	@GetMapping("/profile/{id}")
	public ResponseEntity<DataResponse<UserInformationDTO>> getProfile(@PathVariable("id") Integer id) {

		UserInformationDTO userDto = userService.getProfile(id);
		return ResponseEntity.ok(DataResponse.<UserInformationDTO>builder().status("success") // Set status to "success"
				.message("Thông tin người dùng").data(userDto).build());
	}

	@PutMapping(value = "/profile/update")
	public ResponseEntity<DataResponse<Object>> updateProfile(Principal principal, @Valid @RequestBody UpdateInformationRequest userUpdateRequest) {
	    String username = principal.getName(); 
	    Integer userId = userService.getUserIdByUsername(username);

	    DataResponse<Object> response = userService.updateProfile(userId, userUpdateRequest);
	    return ResponseEntity.status(HttpStatus.OK).body(response);
	}

}
