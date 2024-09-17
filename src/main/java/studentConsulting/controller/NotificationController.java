package studentConsulting.controller;

import java.security.Principal;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.constant.SecurityService;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.INotificationService;
import studentConsulting.service.IUserService;

@RestController
@RequestMapping("${base.url}")
public class NotificationController {
	
	@Autowired
    private INotificationService notificationService;  
    
	@Autowired
    private SecurityService securityService;
    
    @Autowired
    private UserRepository userRepository;
	@GetMapping("/notification")
	public ResponseEntity<DataResponse<List<NotificationEntity>>> getUserNotifications(Principal principal) {
	    String username = principal.getName();
	    Optional<UserInformationEntity> userOpt = securityService.getAuthenticatedUser(username, userRepository);
	    UserInformationEntity user = userOpt.get();
	    List<NotificationEntity> notifications = notificationService.getNotificationsByReceiverId(user.getId());

	    if (notifications.isEmpty()) {
	        throw new ErrorException("Không có thông báo nào cho người dùng");
	    }
	    
	    DataResponse<List<NotificationEntity>> response = DataResponse.<List<NotificationEntity>>builder()
	        .status("success")
	        .message("Danh sách thông báo của người dùng")
	        .data(notifications)
	        .build();

	    return ResponseEntity.ok(response);
	}


}