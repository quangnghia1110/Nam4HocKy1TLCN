package studentConsulting.controller;

import java.security.Principal;

import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.UserInformationDTO;
import studentConsulting.model.payload.request.authentication.ChangePasswordRequest;
import studentConsulting.model.payload.request.authentication.UpdateInformationRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.implement.UserServiceImpl;

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
        return ResponseEntity.ok(DataResponse.<UserInformationDTO>builder()
                .status("success")
                .message("Thông tin người dùng")
                .data(userDto)
                .build());
    }

	@PutMapping(value = "/profile/update")
	public ResponseEntity<DataResponse<Object>> updateProfile(Principal principal, @Valid @RequestBody UpdateInformationRequest userUpdateRequest) {
		
		String username = principal.getName(); 
	    Integer userId = userService.getUserIdByUsername(username);

	    DataResponse<Object> response = userService.updateProfile(userId, userUpdateRequest);
	    return ResponseEntity.status(HttpStatus.OK).body(response);
	}

}
