package studentConsulting.controller.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.model.entity.NotificationEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.interfaces.common.INotificationService;

import java.security.Principal;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class NotificationController {

    @Autowired
    private INotificationService notificationService;

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

//    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
//    @GetMapping("/user/notifications/list")
//    public ResponseEntity<DataResponse<Page<NotificationDTO>>> getUserNotifications(Principal principal,
//                                                                                    @RequestParam(required = false) String content,
//                                                                                    @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
//                                                                                    @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
//                                                                                    @RequestParam(defaultValue = "0") int page,
//                                                                                    @RequestParam(defaultValue = "10") int size,
//                                                                                    @RequestParam(defaultValue = "time") String sortBy,
//                                                                                    @RequestParam(defaultValue = "desc") String sortDir) {
//
//        String email = principal.getName();
//        System.out.println("Email: " + email);
//        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
//        if (!userOpt.isPresent()) {
//            throw new Exceptions.ErrorException("Không tìm thấy người dùng");
//        }
//        UserInformationEntity user = userOpt.get();
//
//        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
//        Page<NotificationDTO> notifications = notificationService.findNotificationsByUserWithFilters(user.getId(), content, startDate, endDate, pageable);
//
//        if (notifications.isEmpty()) {
//            return ResponseEntity.ok(DataResponse.<Page<NotificationDTO>>builder()
//                    .status("success")
//                    .message("Không có thông báo nào")
//                    .build());
//        }
//        return ResponseEntity.ok(DataResponse.<Page<NotificationDTO>>builder()
//                .status("success")
//                .message("Lấy danh sách thông báo thành công.")
//                .data(notifications)
//                .build());
//    }
}
