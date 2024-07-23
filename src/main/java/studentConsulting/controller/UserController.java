package studentConsulting.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import studentConsulting.model.payload.request.authentication.*;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.model.payload.response.LoginResponse;
import studentConsulting.model.payload.response.RegisterResponse;
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
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Password changed successfully", content = @Content(schema = @Schema(implementation = DataResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public ResponseEntity<DataResponse<Object>> changePassword(Principal principal, @Valid @RequestBody ChangePasswordRequest changePasswordRequest) {
        DataResponse<Object> response = userService.changePassword(principal.getName(), changePasswordRequest);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @GetMapping(value = "/profile")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Profile fetched successfully", content = @Content(schema = @Schema(implementation = DataResponse.class))),
        @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public ResponseEntity<DataResponse<Object>> getProfile(UserPrincipal userPrincipal) {
        DataResponse<Object> response = userService.getProfile(userPrincipal.getUserId());
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @PutMapping(value = "/profile/update")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Profile updated successfully", content = @Content(schema = @Schema(implementation = DataResponse.class))),
        @ApiResponse(responseCode = "400", description = "Bad request", content = @Content),
        @ApiResponse(responseCode = "500", description = "Internal server error", content = @Content)
    })
    public ResponseEntity<DataResponse<Object>> updateProfile(UserPrincipal userPrincipal, @Valid @RequestBody UpdateInformationRequest userUpdateRequest) {
        DataResponse<Object> response = userService.updateProfile(userPrincipal.getUserId(), userUpdateRequest);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }
}
