package studentConsulting.controller;

import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.constant.SecurityService;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.PostDTO;
import studentConsulting.model.payload.request.news.CreatePostRequest;
import studentConsulting.model.payload.request.news.UpdatePostRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.INotificationService;
import studentConsulting.service.IPostService;
import studentConsulting.service.IUserService;

@RestController
@RequestMapping("${base.url}")
public class PostController {

    @Autowired
    private IPostService postService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private IUserService userService;
    
    @Autowired
    private INotificationService notificationService;  
    
    @Autowired
    private SecurityService securityService;
    
    @PreAuthorize("hasRole('TUVANVIEN') or hasRole('TRUONGBANTUVAN')")
    @PostMapping("/post")
    public ResponseEntity<DataResponse<PostDTO>> createPost(
            @ModelAttribute CreatePostRequest postRequest,
            Principal principal) {

        String username = principal.getName();
        Optional<UserInformationEntity> userOpt = securityService.getAuthenticatedUser(username, userRepository);

        UserInformationEntity user = userOpt.get();
        Integer userId = user.getId();

        PostDTO postDTO = postService.createPost(postRequest, userId).getData();

        UserInformationEntity admin = userRepository.findAdmin();

        NotificationEntity notification = NotificationEntity.builder()
                .senderId(user.getId())  
                .receiverId(admin.getId()) 
                .content(NotificationContent.NEW_POST.formatMessage(user.getLastName() + " " + user.getFirstName()))                .time(LocalDateTime.now())
                .notificationType(NotificationType.ADMIN)
                .status(NotificationStatus.UNREAD)
                .build();
        notificationService.sendNotification(notification);

        return ResponseEntity.ok(DataResponse.<PostDTO>builder()
                .status("success")
                .message("Tạo bài viết thành công.")
                .data(postDTO)
                .build());
    }

    
    @PreAuthorize("hasRole('TUVANVIEN') or hasRole('TRUONGBANTUVAN')")
    @PutMapping("/post/update")
    public ResponseEntity<DataResponse<PostDTO>> updatePost(
            @RequestParam Integer id,
            @ModelAttribute UpdatePostRequest postRequest,
            Principal principal) {

        String username = principal.getName();
        Optional<UserInformationEntity> userOptional = userRepository.findByAccountUsername(username);

        if (userOptional.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng.");
        }

        UserInformationEntity user = userOptional.get();
        Integer userId = user.getId();

        DataResponse<PostDTO> response = postService.updatePost(id, postRequest, userId);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @PreAuthorize("hasRole('TUVANVIEN') or hasRole('TRUONGBANTUVAN')")
    @DeleteMapping("/post/delete")
    public ResponseEntity<DataResponse<String>> deletePost(@RequestParam Integer id, Principal principal) {
        String username = principal.getName();
        Optional<UserInformationEntity> userOptional = userRepository.findByAccountUsername(username);
        
        if (userOptional.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng.");
        }

        UserInformationEntity user = userOptional.get();
        Integer userId = user.getId();
        
        postService.deletePost(id, userId);
        
        return new ResponseEntity<>(DataResponse.<String>builder()
                .status("success")
                .message("Bài viết đã được xóa thành công")
                .build(), HttpStatus.OK);
    }

    @PreAuthorize("hasRole('TUVANVIEN') or hasRole('TRUONGBANTUVAN')")
    @GetMapping("/post/pending")
    public ResponseEntity<DataResponse<List<PostDTO>>> getPendingPosts(Principal principal) {
        String username = principal.getName();
        Optional<UserInformationEntity> userOpt = securityService.getAuthenticatedUser(username, userRepository);

        UserInformationEntity user = userOpt.get();
        String userId = user.getId().toString();

        DataResponse<List<PostDTO>> response = postService.getPendingPostsByUser(userId);

        if (response.getData().isEmpty()) {
            throw new ErrorException("Không có bài viết nào đang chờ duyệt.");
        }

        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    
    @PreAuthorize("hasRole('ADMIN')")
    @PostMapping("/admin/post/approve")
    public ResponseEntity<DataResponse<PostDTO>> approvePost(@RequestParam Integer id, Principal principal) {
        
        PostDTO postDTO = postService.approvePost(id).getData();

        Optional<UserInformationEntity> postOwnerOpt = userRepository.findById(postDTO.getUserId());
        if (postOwnerOpt.isEmpty()) {
            throw new ErrorException("Người tạo bài viết không tồn tại.");
        }

        UserInformationEntity postOwner = postOwnerOpt.get();

        Optional<UserInformationEntity> currentUserOpt = userRepository.findByAccountUsername(principal.getName());
        if (currentUserOpt.isEmpty()) {
            throw new ErrorException("Người đăng nhập không tồn tại.");
        }

        UserInformationEntity currentUser = currentUserOpt.get();
        NotificationType notificationType = null;
        if (postOwner.getAccount().getRole().getName().contains("ROLE_TUVANVIEN")) { 
            notificationType = NotificationType.TUVANVIEN;
        } else if (postOwner.getAccount().getRole().getName().contains("ROLE_TRUONGBANTUVAN")) { 
            notificationType = NotificationType.TRUONGBANTUVAN;
        }
        
        NotificationEntity notification = NotificationEntity.builder()
                .senderId(currentUser.getId())  
                .receiverId(postOwner.getId())  
                .content(NotificationContent.APPROVE_POST.formatMessage(currentUser.getLastName() + " " + currentUser.getFirstName()))  
                .time(LocalDateTime.now())  
                .notificationType(notificationType)  
                .status(NotificationStatus.UNREAD)  
                .build();

        notificationService.sendNotification(notification);

        // Trả về phản hồi với DataResponse
        return ResponseEntity.ok(
            DataResponse.<PostDTO>builder()
                .status("success")
                .message("Bài viết đã được duyệt thành công.")
                .data(postDTO)
                .build());
    }


}

