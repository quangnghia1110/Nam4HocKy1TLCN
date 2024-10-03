package studentConsulting.controller.user;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.payload.dto.notification.NotificationDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.user.IUserNotificationService;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class UserNotificationController {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private IUserNotificationService notificationService;

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @GetMapping("/user/notifications/list")
    public ResponseEntity<DataResponse<Page<NotificationDTO>>> getUserNotifications(Principal principal,
                                                                                    @RequestParam(required = false) String content,
                                                                                    @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
                                                                                    @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
                                                                                    @RequestParam(defaultValue = "0") int page,
                                                                                    @RequestParam(defaultValue = "10") int size,
                                                                                    @RequestParam(defaultValue = "time") String sortBy,
                                                                                    @RequestParam(defaultValue = "desc") String sortDir) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new Exceptions.ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<NotificationDTO> notifications = notificationService.findNotificationsByUserWithFilters(user.getId(), content, startDate, endDate, pageable);

        if (notifications.isEmpty()) {
            return ResponseEntity.ok(DataResponse.<Page<NotificationDTO>>builder()
                    .status("success")
                    .message("Không có thông báo nào")
                    .build());
        }
        return ResponseEntity.ok(DataResponse.<Page<NotificationDTO>>builder()
                .status("success")
                .message("Lấy danh sách thông báo thành công.")
                .data(notifications)
                .build());
    }
}

