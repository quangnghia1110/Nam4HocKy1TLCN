package studentConsulting.controller.actor;

import com.lowagie.text.DocumentException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.FilePaths;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.content.CommentEntity;
import studentConsulting.model.entity.content.PostEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.content.CommentDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.content.CommentRepository;
import studentConsulting.repository.content.PostRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.actor.ICommentService;
import studentConsulting.service.interfaces.common.IExcelService;
import studentConsulting.service.interfaces.common.INotificationService;
import studentConsulting.service.interfaces.common.IPdfService;

import javax.servlet.http.HttpServletResponse;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.Principal;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class CommentController {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommentService commentService;

    @Autowired
    private IExcelService excelService;

    @Autowired
    private IPdfService pdfService;

    @Autowired
    private INotificationService notificationService;

    @Autowired
    private PostRepository postRepository;

    @Autowired
    private CommentRepository commentRepository;

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/comment/create")
    public ResponseEntity<DataResponse<CommentDTO>> createComment(@RequestParam Integer postId,
                                                                  @RequestParam String text, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        CommentDTO createdComment = commentService.createComment(postId, text, email);
        Optional<PostEntity> postOpt = postRepository.findById(postId);
        if (postOpt.isPresent()) {
            UserInformationEntity postOwner = postOpt.get().getUser();

            notificationService.sendUserNotification(
                    userOpt.get().getId(),
                    postOwner.getId(),
                    NotificationContent.NEW_COMMENT.formatMessage(userOpt.get().getLastName() + " " + userOpt.get().getFirstName()),
                    NotificationType.USER
            );
        }
        return ResponseEntity.ok(DataResponse.<CommentDTO>builder().status("success")
                .message("Bình luận đã được tạo thành công").data(createdComment).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/comment/reply")
    public ResponseEntity<DataResponse<CommentDTO>> replyComment(@RequestParam Integer commentFatherId,
                                                                 @RequestParam String text, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        CommentDTO replyComment = commentService.replyComment(commentFatherId, text, email);

        Optional<CommentEntity> commentOpt = commentRepository.findById(commentFatherId);
        if (commentOpt.isPresent()) {
            CommentEntity originalComment = commentOpt.get();
            UserInformationEntity commentOwner = originalComment.getUserComment();
            PostEntity post = originalComment.getPost();
            UserInformationEntity postOwner = post.getUser();

            notificationService.sendUserNotification(
                    userOpt.get().getId(),
                    postOwner.getId(),
                    NotificationContent.NEW_REPLY_POST.formatMessage(userOpt.get().getLastName() + " " + userOpt.get().getFirstName()),
                    NotificationType.USER
            );

            if (!commentOwner.getId().equals(postOwner.getId())) {
                notificationService.sendUserNotification(
                        userOpt.get().getId(),
                        commentOwner.getId(),
                        NotificationContent.NEW_REPLY_COMMENT.formatMessage(userOpt.get().getLastName() + " " + userOpt.get().getFirstName()),
                        NotificationType.USER
                );
            }
        }

        return ResponseEntity.ok(DataResponse.<CommentDTO>builder()
                .status("success")
                .message("Bình luận đã được trả lời thành công")
                .data(replyComment)
                .build());
    }


    private boolean isAdminFromDB(String email) {
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (userOpt.isPresent()) {
            UserInformationEntity user = userOpt.get();
            return user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        }
        return false;
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/comment/update")
    public ResponseEntity<DataResponse<CommentDTO>> updateComment(@RequestParam Integer commentId,
                                                                  @RequestParam String text,
                                                                  Principal principal) {

        String email = principal.getName();
        boolean isAdmin = isAdminFromDB(email);

        CommentDTO updatedComment;
        if (isAdmin) {
            updatedComment = commentService.adminUpdateComment(commentId, text);
        } else {
            updatedComment = commentService.updateComment(commentId, text, email);
        }

        return ResponseEntity.ok(DataResponse.<CommentDTO>builder()
                .status("success")
                .message("Bình luận đã được cập nhật thành công")
                .data(updatedComment)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/comment/delete")
    public ResponseEntity<DataResponse<Void>> deleteComment(@RequestParam Integer commentId, Principal principal) {

        String email = principal.getName();
        boolean isAdmin = isAdminFromDB(email);

        if (isAdmin) {
            commentService.adminDeleteComment(commentId);
        } else {
            commentService.deleteComment(commentId, email);
        }

        return ResponseEntity
                .ok(DataResponse.<Void>builder().status("success").message("Bình luận đã được xóa").build());
    }

    @GetMapping("/comment/get-comment-by-post")
    public ResponseEntity<DataResponse<List<CommentDTO>>> getCommentsByPost(@RequestParam Integer postId) {
        DataResponse<List<CommentDTO>> response = commentService.getAllComments(postId);

        if (response.getData().isEmpty()) {
            throw new ErrorException("Không có bình luận nào cho bài viết này.");
        }

        return ResponseEntity.ok(response);
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-comment-csv")
    public void exportCommentsToCsv(
            @RequestParam(required = false) Integer postId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createDate") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<CommentDTO> commentPage = commentService.getCommentsByPostWithPagingAndFilters(Optional.ofNullable(postId), Optional.ofNullable(startDate), Optional.ofNullable(endDate), pageable);
        List<CommentDTO> comments = commentPage.getContent();

        if (comments.isEmpty()) {
            throw new ErrorException("Không có bình luận nào để xuất");
        }

        List<String> headers = List.of("Content", "User ID", "Created At", "Post ID");

        List<List<String>> data = comments.stream()
                .map(comment -> List.of(
                        comment.getText() != null ? comment.getText() : "N/A",
                        comment.getUser().getId() != null ? comment.getUser().getId().toString() : "N/A",
                        comment.getCreate_date() != null ? comment.getCreate_date().toString() : "N/A",
                        comment.getPostId() != null ? comment.getPostId().toString() : "N/A"
                ))
                .collect(Collectors.toList());

        String fileName = "Comments_" + excelService.currentDate() + ".csv";

        excelService.generateExcelFile("Comments", headers, data, fileName, response);
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-comment-pdf")
    public void exportCommentsToPdf(
            @RequestParam(required = false) Integer postId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createDate") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws DocumentException, IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<CommentDTO> commentPage = commentService.getCommentsByPostWithPagingAndFilters(Optional.ofNullable(postId), Optional.ofNullable(startDate), Optional.ofNullable(endDate), pageable);
        List<CommentDTO> comments = commentPage.getContent();

        if (comments.isEmpty()) {
            throw new IOException("Không có bình luận nào để xuất");
        }

        String templatePath = "/templates/comment_template.html";
        String dataRows = buildCommentDataRows(comments);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{comments}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Comments_" + pdfService.currentDate() + ".pdf";
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


    private String buildCommentDataRows(List<CommentDTO> comments) {
        StringBuilder dataRows = new StringBuilder();

        for (CommentDTO comment : comments) {
            dataRows.append("<tr>")
                    .append("<td>").append(comment.getText()).append("</td>")
                    .append("<td>").append(comment.getUser().getName()).append("</td>")
                    .append("<td>").append(comment.getCreate_date()).append("</td>")
                    .append("<td>").append(comment.getPostId()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

}
