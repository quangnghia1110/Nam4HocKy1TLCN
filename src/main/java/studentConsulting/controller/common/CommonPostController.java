package studentConsulting.controller.common;

import com.lowagie.text.DocumentException;
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
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.FilePaths;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.content.PostDTO;
import studentConsulting.model.payload.dto.notification.NotificationResponseDTO;
import studentConsulting.model.payload.request.content.CreatePostRequest;
import studentConsulting.model.payload.request.content.UpdatePostRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
import studentConsulting.service.interfaces.common.ICommonPdfService;
import studentConsulting.service.interfaces.common.ICommonPostService;

import javax.servlet.http.HttpServletResponse;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class CommonPostController {

    @Autowired
    private ICommonPostService postService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonNotificationService notificationService;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    private boolean isAdmin(UserInformationEntity user) {
        return user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/post")
    public ResponseEntity<DataResponse<PostDTO>> createPost(@ModelAttribute CreatePostRequest postRequest,
                                                            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer userId = user.getId();

        PostDTO postDTO = postService.createPost(postRequest, userId).getData();

        UserInformationEntity admin = userRepository.findAdmin();
        sendNotificationToAdmin(user, admin);

        return ResponseEntity.ok(DataResponse.<PostDTO>builder().status("success").message("Tạo bài viết thành công.")
                .data(postDTO).build());
    }

    private void sendNotificationToAdmin(UserInformationEntity sender, UserInformationEntity admin) {
        NotificationEntity notification = NotificationEntity.builder()
                .senderId(sender.getId())
                .receiverId(admin.getId())
                .content(NotificationContent.NEW_POST.formatMessage(sender.getLastName() + " " + sender.getFirstName()))
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

        notificationService.sendNotification(notificationDTO);
        simpMessagingTemplate.convertAndSendToUser(String.valueOf(admin.getId()), "/notification", notificationDTO);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/post/update")
    public ResponseEntity<DataResponse<PostDTO>> updatePost(@RequestParam Integer id,
                                                            @ModelAttribute UpdatePostRequest postRequest,
                                                            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = isAdmin(user);

        if (!isAdmin) {
            PostDTO post = postService.getPostById(id, user.getId()).getData();
            if (!post.getUserId().equals(user.getId())) {
                throw new ErrorException("Bạn chỉ có thể chỉnh sửa bài viết của mình.");
            }
        }

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
        boolean isAdmin = isAdmin(user);

        if (!isAdmin) {
            PostDTO post = postService.getPostById(id, user.getId()).getData();
            if (!post.getUserId().equals(user.getId())) {
                throw new ErrorException("Bạn chỉ có thể xóa bài viết của mình.");
            }
        }

        postService.deletePost(id, user.getId());

        return new ResponseEntity<>(
                DataResponse.<String>builder().status("success").message("Bài viết đã được xóa thành công").build(),
                HttpStatus.OK);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/post/list")
    public ResponseEntity<DataResponse<Page<PostDTO>>> getPosts(
            @RequestParam(required = false) String userName,
            @RequestParam(defaultValue = "true") boolean isApproved,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<PostDTO> posts = postService.getPostsWithFilters(
                Optional.ofNullable(userName),
                isApproved,
                Optional.ofNullable(startDate),
                Optional.ofNullable(endDate),
                pageable);

        if (posts.isEmpty()) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
                    DataResponse.<Page<PostDTO>>builder()
                            .status("error")
                            .message(isApproved ? "Không có bài đăng đã duyệt nào." : "Không có bài đăng chờ duyệt nào.")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<PostDTO>>builder()
                        .status("success")
                        .message(isApproved ? "Lấy danh sách các bài đăng đã duyệt thành công" : "Lấy danh sách các bài đăng chờ duyệt thành công")
                        .data(posts)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-post-csv")
    public void exportPostsToCsv(
            @RequestParam(required = false) String userName,
            @RequestParam(defaultValue = "true") boolean isApproved,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<PostDTO> postPage = postService.getPostsWithFilters(
                Optional.ofNullable(userName),
                isApproved,
                Optional.ofNullable(startDate),
                Optional.ofNullable(endDate),
                pageable);

        List<PostDTO> posts = postPage.getContent();

        if (posts.isEmpty()) {
            throw new ErrorException("Không có bài viết nào để xuất");
        }

        List<String> headers = List.of("Content", "User ID", "Anonymous", "Created At", "File Name", "Approved", "Views");

        List<List<String>> data = posts.stream()
                .map(post -> List.of(
                        post.getContent() != null ? post.getContent() : "N/A",
                        post.getUserId() != null ? post.getUserId().toString() : "N/A",
                        Boolean.toString(post.isAnonymous()),
                        post.getCreatedAt() != null ? post.getCreatedAt().toString() : "N/A",
                        post.getFileName() != null ? post.getFileName() : "N/A",
                        Boolean.toString(post.isApproved()),
                        Integer.toString(post.getViews())
                ))
                .collect(Collectors.toList());

        String fileName = "Posts_" + pdfService.currentDate() + ".csv";

        excelService.generateExcelFile("Posts", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-post-pdf")
    public void exportPostsToPdf(
            @RequestParam(required = false) String userName,
            @RequestParam(defaultValue = "true") boolean isApproved,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws DocumentException, IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<PostDTO> postPage = postService.getPostsWithFilters(
                Optional.ofNullable(userName),
                isApproved,
                Optional.ofNullable(startDate),
                Optional.ofNullable(endDate),
                pageable);

        List<PostDTO> posts = postPage.getContent();

        if (posts.isEmpty()) {
            throw new IOException("Không có bài viết nào để xuất");
        }

        String templatePath = "/templates/post_template.html";
        String dataRows = buildPostDataRows(posts);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{posts}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Posts_" + pdfService.currentDate() + ".pdf";
        String outputFilePath = FilePaths.PDF_OUTPUT_DIRECTORY + fileName;

        try (OutputStream fileOutputStream = new FileOutputStream(outputFilePath)) {
            pdfService.generatePdfFromTemplate(templatePath, placeholders, fileOutputStream);
        } catch (IOException | DocumentException e) {
            throw new IOException("Lỗi khi tạo hoặc lưu file PDF", e);
        }

        try (OutputStream responseStream = response.getOutputStream()) {
            pdfService.generatePdfFromTemplate(templatePath, placeholders, responseStream);
            response.flushBuffer();
        } catch (IOException | DocumentException e) {
            throw new IOException("Lỗi khi gửi file PDF qua HTTP response", e);
        }
    }

    private String buildPostDataRows(List<PostDTO> posts) {
        StringBuilder dataRows = new StringBuilder();

        for (PostDTO post : posts) {
            dataRows.append("<tr>")
                    .append("<td>").append(post.getContent()).append("</td>")
                    .append("<td>").append(post.getUserId()).append("</td>")
                    .append("<td>").append(post.isAnonymous()).append("</td>")
                    .append("<td>").append(post.getCreatedAt()).append("</td>")
                    .append("<td>").append(post.getFileName()).append("</td>")
                    .append("<td>").append(post.isApproved()).append("</td>")
                    .append("<td>").append(post.getViews()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/import-post-csv")
    public ResponseEntity<?> importPostFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        postService.importPost(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }
}
