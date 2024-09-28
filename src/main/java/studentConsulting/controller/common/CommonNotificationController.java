package studentConsulting.controller.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.interfaces.common.ICommonNotificationService;

import java.security.Principal;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class CommonNotificationController {

    @Autowired
    private ICommonNotificationService notificationService;

    @Autowired
    private UserRepository userRepository;

    @GetMapping("/notification")
    public ResponseEntity<DataResponse<List<NotificationEntity>>> getUserNotifications(Principal principal) {
        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        List<NotificationEntity> notifications = notificationService.getNotificationsByReceiverId(user.getId());


        DataResponse<List<NotificationEntity>> response = DataResponse.<List<NotificationEntity>>builder()
                .status("success")
                .message("Danh sách thông báo của người dùng")
                .data(notifications)
                .build();

        return ResponseEntity.ok(response);
    }


}
