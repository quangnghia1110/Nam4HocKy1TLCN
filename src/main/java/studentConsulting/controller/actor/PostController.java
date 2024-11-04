package studentConsulting.controller.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.PostDTO;
import studentConsulting.model.payload.request.CreatePostRequest;
import studentConsulting.model.payload.request.UpdatePostRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.interfaces.actor.IPostService;
import studentConsulting.service.interfaces.common.IExcelService;
import studentConsulting.service.interfaces.common.INotificationService;
import studentConsulting.service.interfaces.common.IPdfService;

import java.security.Principal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class PostController {

    @Autowired
    private IPostService postService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private INotificationService notificationService;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @Autowired
    private IExcelService excelService;

    @Autowired
    private IPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/post")
    public ResponseEntity<DataResponse<PostDTO>> createPost(@ModelAttribute CreatePostRequest postRequest, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        PostDTO postDTO = postService.createPost(postRequest, user.getId()).getData();

        List<UserInformationEntity> admins = userRepository.findAllByRole(SecurityConstants.Role.ADMIN);

        for (UserInformationEntity admin : admins) {
            notificationService.sendUserNotification(
                    user.getId(),
                    admin.getId(),
                    NotificationContent.NEW_POST_CREATED.formatMessage(user.getLastName() + " " + user.getFirstName()),
                    NotificationType.ADMIN
            );
        }

        return ResponseEntity.ok(DataResponse.<PostDTO>builder()
                .status("success")
                .message("Tạo bài viết thành công.")
                .data(postDTO)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/post/list")
    public ResponseEntity<DataResponse<Page<PostDTO>>> getPosts(
            @RequestParam boolean isApproved,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        UserInformationEntity user = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        String userRole = user.getAccount().getRole().getName();
        Page<PostDTO> posts;

        if (userRole.equals(SecurityConstants.Role.USER)) {
            posts = postService.getAllPostsWithFilters(true, Optional.ofNullable(startDate), Optional.ofNullable(endDate), pageable);
        } else if (userRole.equals(SecurityConstants.Role.ADMIN)) {
            posts = postService.getAllPostsWithFilters(isApproved, Optional.ofNullable(startDate), Optional.ofNullable(endDate), pageable);
        } else if (userRole.equals(SecurityConstants.Role.TRUONGBANTUVAN) || userRole.equals(SecurityConstants.Role.TUVANVIEN)) {
            posts = postService.getPostsWithFiltersByRole(isApproved, Optional.ofNullable(startDate), Optional.ofNullable(endDate), pageable, principal);
        } else {
            throw new ErrorException("Bạn không có quyền truy cập vào danh sách bài viết.");
        }

        if (posts.isEmpty()) {
            String message = "Không có bài đăng nào";
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(DataResponse.<Page<PostDTO>>builder().status("error").message(message).build());
        }

        String message = "Lấy danh sách các bài đăng thành công";
        return ResponseEntity.ok(DataResponse.<Page<PostDTO>>builder().status("success").message(message).data(posts).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/post/update")
    public ResponseEntity<DataResponse<PostDTO>> updatePost(@RequestParam Integer id, @ModelAttribute UpdatePostRequest postRequest, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        DataResponse<PostDTO> response = postService.updatePost(id, postRequest, user.getId());
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/post/delete")
    public ResponseEntity<DataResponse<String>> deletePost(@RequestParam Integer id, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        postService.deletePost(id, user.getId());

        return new ResponseEntity<>(DataResponse.<String>builder().status("success").message("Bài viết đã được xóa thành công").build(), HttpStatus.OK);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/post/detail")
    public ResponseEntity<DataResponse<PostDTO>> getPostDetail(@RequestParam Integer id, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        DataResponse<PostDTO> postDetail = postService.getPostById(id, user.getId());
        return ResponseEntity.ok(postDetail);
    }
}
