package studentConsulting.controller.consultant;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.NotificationResponseDTO;
import studentConsulting.model.payload.dto.PostDTO;
import studentConsulting.model.payload.request.news.CreatePostRequest;
import studentConsulting.model.payload.request.news.UpdatePostRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
import studentConsulting.service.interfaces.common.ICommonUserService;
import studentConsulting.service.interfaces.consultant.IConsultantPostService;

import java.security.Principal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class ConsultantPostController {

    @Autowired
    private IConsultantPostService postService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonUserService userService;

    @Autowired
    private ICommonNotificationService notificationService;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @PostMapping("/post")
    public ResponseEntity<DataResponse<PostDTO>> createPost(@ModelAttribute CreatePostRequest postRequest,
                                                            Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer userId = user.getId();

        PostDTO postDTO = postService.createPost(postRequest, userId).getData();

        UserInformationEntity admin = userRepository.findAdmin();

        NotificationEntity notification = NotificationEntity.builder()
                .senderId(user.getId())
                .receiverId(admin.getId())
                .content(NotificationContent.NEW_POST.formatMessage(user.getLastName() + " " + user.getFirstName()))
                .time(LocalDateTime.now())
                .notificationType(NotificationType.ADMIN)
                .status(NotificationStatus.UNREAD)
                .build();

        NotificationResponseDTO.NotificationDTO notificationDTO = NotificationResponseDTO.NotificationDTO.builder()
                .senderId(notification.getSenderId())
                .receiverId(notification.getReceiverId())
                .content(notification.getContent())
                .time(notification.getTime())
                .notificationType(notification.getNotificationType().name())
                .status(notification.getStatus().name())
                .build();

        NotificationResponseDTO responseDTO = NotificationResponseDTO.builder()
                .status("notification")
                .data(notificationDTO)
                .build();

        notificationService.sendNotification(notificationDTO);
        System.out.println("Payload: " + responseDTO);

        simpMessagingTemplate.convertAndSendToUser(String.valueOf(admin.getId()), "/notification", responseDTO);

        return ResponseEntity.ok(DataResponse.<PostDTO>builder().status("success").message("Tạo bài viết thành công.")
                .data(postDTO).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @PutMapping("/post/update")
    public ResponseEntity<DataResponse<PostDTO>> updatePost(@RequestParam Integer id,
                                                            @ModelAttribute UpdatePostRequest postRequest, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }


        UserInformationEntity user = userOpt.get();
        Integer userId = user.getId();

        DataResponse<PostDTO> response = postService.updatePost(id, postRequest);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @DeleteMapping("/post/delete")
    public ResponseEntity<DataResponse<String>> deletePost(@RequestParam Integer id, Principal principal) {
        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer userId = user.getId();

        postService.deletePost(id, userId);

        return new ResponseEntity<>(
                DataResponse.<String>builder().status("success").message("Bài viết đã được xóa thành công").build(),
                HttpStatus.OK);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/post/pending")
    public ResponseEntity<DataResponse<List<PostDTO>>> getPendingPosts(Principal principal) {
        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        String userId = user.getId().toString();

        DataResponse<List<PostDTO>> response = postService.getPendingPostsByUser(userId);

        if (response.getData().isEmpty()) {
            throw new ErrorException("Không có bài viết nào đang chờ duyệt.");
        }

        return new ResponseEntity<>(response, HttpStatus.OK);
    }
}
