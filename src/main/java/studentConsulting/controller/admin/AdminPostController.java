package studentConsulting.controller.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.content.PostDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.admin.IAdminPostService;
import studentConsulting.service.interfaces.common.INotificationService;
import studentConsulting.service.interfaces.common.IUserService;

import java.security.Principal;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdminPostController {

    @Autowired
    private IAdminPostService postService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private IUserService userService;

    @Autowired
    private INotificationService notificationService;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/post/approve")
    public ResponseEntity<DataResponse<PostDTO>> approvePost(@RequestParam Integer id, Principal principal) {

        PostDTO postDTO = postService.approvePost(id).getData();

        Optional<UserInformationEntity> postOwnerOpt = userRepository.findById(postDTO.getUserId());
        if (postOwnerOpt.isEmpty()) {
            throw new ErrorException("Người tạo bài viết không tồn tại.");
        }

        UserInformationEntity postOwner = postOwnerOpt.get();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(principal.getName());
        UserInformationEntity user = userOpt.orElseThrow(() -> new ErrorException("Người dùng không tồn tại."));

        NotificationType notificationType = null;
        if (postOwner.getAccount().getRole().getName().contains(SecurityConstants.Role.TUVANVIEN)) {
            notificationType = NotificationType.TUVANVIEN;
        } else if (postOwner.getAccount().getRole().getName().contains(SecurityConstants.Role.TRUONGBANTUVAN)) {
            notificationType = NotificationType.TRUONGBANTUVAN;
        }

        notificationService.sendUserNotification(
                user.getId(),
                postOwner.getId(),
                NotificationContent.APPROVE_POST.formatMessage(user.getLastName() + " " + user.getFirstName()),
                notificationType
        );

        return ResponseEntity.ok(DataResponse.<PostDTO>builder().status("success")
                .message("Bài viết đã được duyệt thành công.").data(postDTO).build());
    }

}
